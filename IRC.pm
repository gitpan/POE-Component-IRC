# $Id: IRC.pm,v 1.11 1999/12/12 11:48:07 dennis Exp $
#
# POE::Component::IRC, by Dennis Taylor <dennis@funkplanet.com>
#
# This module may be used, modified, and distributed under the same
# terms as Perl itself. Please see the license that came with your Perl
# distribution for details.
#

package POE::Component::IRC;

use strict;
use POE;
use POE::Session;
use POE::Wheel::SocketFactory;
use POE::Wheel::ReadWrite;
use POE::Driver::SysRW;
use POE::Filter::Line;
use POE::Filter::IRC;
use POE::Filter::CTCP;
use Carp;
use Socket;
use Sys::Hostname;
use Symbol;
use vars qw($VERSION);

use constant BLOCKSIZE => 1024;           # Send DCC data in 2k chunks
use constant INCOMING_BLOCKSIZE => 10240; # 10k per DCC socket read

$VERSION = '1.0b';
my $debug;


# Start a DCC SEND or CHAT connection with a remote host.
sub _dcc_listen_accept {
  my ($kernel, $heap, $sock) = @_[KERNEL, HEAP, ARG0];
  my ($addr, $buf);
  
  my %conn = %{$heap->{dcc_listen}->{$sock}};
  my $newsock = gensym();
  
  # Accept the incoming connection.
  unless ($addr = accept( $newsock, $sock )) {
    _send_event( $kernel, $heap, 'irc_dcc_error', "accept failed: $!",
		 @conn{'nick', 'type', 0, 'file'} );
    $kernel->select( $sock, undef, undef, undef );
    delete $heap->{dcc_listen}->{$sock};
    close $sock;
    return;
  }
  $conn{'port'} = (unpack_sockaddr_in( $newsock ))[0];
  $_ = select $newsock;   $| = 1;   select $_;
  
  if ($conn{'type'} eq 'SEND') {
    # Send the first packet to get the ball rolling.
    unless (open FILE, $conn{'file'}) {
      _send_event( $kernel, $heap, 'irc_dcc_error',
		   "Can't open $conn{'file'} for DCC SEND: $!",
		   @conn{'nick', 'type', 'port', 'file'} );
      $kernel->select( $sock, undef, undef, undef );
      delete $heap->{dcc_listen}->{$sock};
      close $sock;
      return;
    }
    binmode FILE;
    read FILE, $buf, $conn{'blocksize'};
    send $newsock, $buf, 0;
    
    # Index the connection's state by the new socket. Store the
    # filehandle with the rest of this connection's state.
    $heap->{dcc_open}->{$newsock} = $heap->{dcc_listen}->{$sock};
    $heap->{dcc_open}->{$newsock}->{'fh'} = \*FILE;
    delete $heap->{dcc_listen}->{$sock};
  }
  
  # Destroy the old socket and monitor the new one for incoming data.
  $kernel->select( $newsock, '_dcc_read', undef, undef );
  $kernel->select( $sock, undef, undef, undef );
  close $sock;
  
  # Tell any listening sessions that the connection is up.
  _send_event( $kernel, $heap, 'irc_dcc_start', @conn{'nick', 'type', 'port'},
	       ($conn{'type'} eq 'SEND' ? (@conn{'file', 'size'}) : ()) );
}


# Accept incoming data on a DCC socket.
sub _dcc_read {
  my ($kernel, $heap, $sock) = @_[KERNEL, HEAP, ARG0];
  my %conn = %{$heap->{dcc_open}->{$sock}};
  my $buf;
  
  # Read a chunk of the incoming data
  unless (defined( read $sock, $buf, INCOMING_BLOCKSIZE )) {
    _send_event( $kernel, $heap, 'irc_dcc_error', "socket read failed: $!",
		 @conn{'nick', 'type', 'port', 'file'} );
    $kernel->select( $sock, undef, undef, undef );
    delete $heap->{dcc_listen}->{$sock};
    close $sock;
    return;
  }
  $buf = $conn{'frag'} . $buf if $conn{'frag'};
  
  # Parse it somehow, according to the DCC type, and send an event
  # noting the received transmission to all interested sessions.
  if ($conn{'type'} eq 'GET') {
    print {$conn{'fh'}} $buf;
    $conn{'done'} += length $buf;
    _send_event( $kernel, $heap, 'irc_dcc_get',
		 @conn{ qw(nick port file size done) } );
    
  } elsif ($conn{'type'} eq 'SEND') {
    $_ = length( $buf ) % 4;
    if ($_ > 0) {
      $conn{'frag'} = substr $buf, -$_;
      $buf = substr $buf, 0, -4;
    }
    if (length $buf >= 4) {
      $conn{'done'} = unpack "N", substr( $buf, -4 );
      _send_event( $kernel, $heap, 'irc_dcc_send',
		   @conn{ qw(nick port file size done) } );
    }
    if ($conn{'done'} >= $conn{'size'}) {
      _send_event( $kernel, $heap, 'irc_dcc_done',
		   @conn{ qw(nick type port file size done) } );
    }
    # Send the next 'blocksize'-sized packet.
    read $conn{'fh'}, $buf, $conn{'blocksize'};
    send $sock, $buf, 0;
    
  } elsif ($conn{'type'} eq 'CHAT') {
    my @lines = split /\012/, $buf, -1;
    $lines[-1] .= "\012";
    $conn{'frag'} = ($buf !~ /\012$/) ? pop @lines : '';
    _send_event( $kernel, $heap, 'irc_dcc_chat',
		 @conn{'nick', 'port'}, @lines );
    
  } else {
    _send_event( $kernel, $heap, 'irc_dcc_' . lc $conn{'type'},
		 @conn{'nick', 'port'}, $buf );
  }
}


# Parse a message from the IRC server and generate the appropriate
# event(s) for listening sessions.
sub _parseline {
  my ($kernel, $session, $heap, $line) = @_[KERNEL, SESSION, HEAP, ARG0];
  my (@events, @cooked);
  
  # Feed the proper Filter object the raw IRC text and get the
  # "cooked" events back for sending, then deliver each event. We
  # handle CTCPs separately from normal IRC messages here, to avoid
  # silly module dependencies later.
  
  @cooked = ($line =~ tr/\001// ? @{$heap->{ctcp_filter}->get( [$line] )}
	     : @{$heap->{irc_filter}->get( [$line] )} );
  
  foreach my $ev (@cooked) {
    $ev->{name} = 'irc_' . $ev->{name};
    _send_event( $kernel, $heap, $ev->{name}, @{$ev->{args}} );
  }
}


# Sends an event to all interested sessions. This is a separate sub
# because I do it so much, but it's not an actual POE event because it
# doesn't need to be one and I don't need the overhead.
sub _send_event  {
  my ($kernel, $heap, $event, @args) = @_;
  my %sessions;
  
  foreach (values %{$heap->{events}->{'irc_all'}},
	   values %{$heap->{events}->{$event}}) {
    $sessions{$_} = $_;
  }
  foreach (values %sessions) {
    $kernel->post( $_, $event, @args );
  }
}


# Internal function called when a socket is closed.
sub _sock_down {
  my ($kernel, $heap) = @_[KERNEL, HEAP];
  
  # Destroy the RW wheel for the socket.
  delete $heap->{'socket'};
  $heap->{connected} = 0;
  
  # post a 'irc_disconnected' to each session that cares
  foreach (keys %{$heap->{sessions}}) {
    $kernel->post( $heap->{sessions}->{$_}->{'ref'},
		   'irc_disconnected', $heap->{server} );
  }
}


# Internal function called when a socket fails to be properly opened.
sub _sock_failed {
  my ($kernel, $heap) = @_[KERNEL, HEAP];
  
  _send_event( $kernel, $heap, 'irc_socketerr', $! );
}


# Internal function called when a connection is established.
sub _sock_up {
  my ($kernel, $heap, $session, $socket) = @_[KERNEL, HEAP, SESSION, ARG0];
  
  # We no longer need the SocketFactory wheel. Scrap it.
  delete $heap->{'socketfactory'};
  
  # Create a new ReadWrite wheel for the connected socket.
  $heap->{'socket'} = new POE::Wheel::ReadWrite
    ( Handle     => $socket,
      Driver     => POE::Driver::SysRW->new(),
      Filter     => POE::Filter::Line->new( InputRegexp => '\015?\012' ),
      InputState => '_parseline',
      ErrorState => '_sock_down',
    );
  
  if ($heap->{'socket'}) {
    $heap->{connected} = 1;
  } else {
    _send_event( $kernel, $heap, 'irc_socketerr',
		 "Couldn't create ReadWrite wheel for IRC socket" );
  }
  
  # Post a 'irc_connected' event to each session that cares
  foreach (keys %{$heap->{sessions}}) {
    $kernel->post( $heap->{sessions}->{$_}->{'ref'},
		   'irc_connected', $heap->{server} );
  }
  
  # Now that we're connected, attempt to log into the server.
  if ($heap->{password}) {
    $kernel->call( $session, 'sl', "PASS " . $heap->{password} );
  }
  $kernel->call( $session, 'sl', "NICK " . $heap->{nick} );
  $kernel->call( $session, 'sl', "USER " .
		 join( ' ', $heap->{username},
		       "foo.bar.com",
		       $heap->{server},
		       ':' . $heap->{ircname} ));
}


# Set up the component's IRC session.
sub _start {
  my ($kernel, $session, $heap, $alias, $options) =
    @_[KERNEL, SESSION, HEAP, ARG0, ARG1];
  
  $session->option( @$options ) if @$options;
  $kernel->alias_set($alias);
  $kernel->yield( 'register', 'ping' );
  $heap->{irc_filter} = POE::Filter::IRC->new();
  $heap->{ctcp_filter} = POE::Filter::CTCP->new();
}


# Destroy ourselves when asked politely.
sub _stop {
  my ($kernel, $heap, $quitmsg) = @_[KERNEL, HEAP, ARG0];
  
  if ($heap->{connected}) {
    $kernel->yield( 'quit', $quitmsg );
  }
}


# The handler for commands which have N arguments, separated by commas.
sub commasep {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $args = join ',', @_[ARG0 .. $#_];
  
  $state = uc $state;
  $state .= " $args" if defined $args;
  $kernel->yield( 'sl', $state );
}


# Attempt to connect this component to an IRC server.
sub connect {
  my ($kernel, $heap, $session, $args) = @_[KERNEL, HEAP, SESSION, ARG0];
  
  if ($args) {
    my %arg;
    if (ref $args eq 'ARRAY') {
      %arg = @$args;
    } elsif (ref $args eq 'HASH') {
      %arg = %$args;
    } else {
      die "First argument to connect() should be a hash or array reference";
    }
    
    $heap->{'password'} = $arg{'Password'} if exists $arg{'Password'};
    $heap->{'localaddr'} = $arg{'LocalAddr'} if exists $arg{'LocalAddr'};
    $heap->{'localport'} = $arg{'LocalPort'} if exists $arg{'LocalPort'};
    $heap->{'nick'} = $arg{'Nick'} if exists $arg{'Nick'};
    $heap->{'port'} = $arg{'Port'} if exists $arg{'Port'};
    $heap->{'server'} = $arg{'Server'} if exists $arg{'Server'};
    $heap->{'ircname'} = $arg{'Ircname'} if exists $arg{'Ircname'};
    $heap->{'username'} = $arg{'Username'} if exists $arg{'Username'};
    if (exists $arg{'Debug'}) {
      $heap->{'debug'} = $arg{'Debug'};
      $heap->{irc_filter}->debug( $arg{'Debug'} );
      $heap->{ctcp_filter}->debug( $arg{'Debug'} );
    }
  }
  
  # Make sure that we have reasonable defaults for all the attributes.
  # The "IRC*" variables are ircII environment variables.
  $heap->{'nick'} = $ENV{IRCNICK} || eval { scalar getpwuid($>) } ||
    $ENV{USER} || $ENV{LOGNAME} || "WankerBot"
      unless ($heap->{'nick'});
  $heap->{'username'} = eval { scalar getpwuid($>) } || $ENV{USER} ||
    $ENV{LOGNAME} || "foolio"
      unless ($heap->{'username'});
  $heap->{'ircname'} = $ENV{IRCNAME} || eval { (getpwuid $>)[6] } ||
    "Just Another Perl Hacker"
      unless ($heap->{'ircname'});
  unless ($heap->{'server'}) {
    die "No IRC server specified" unless $ENV{IRCSERVER};
    $heap->{'server'} = $ENV{IRCSERVER};
  }
  $heap->{'port'} = 6667 unless $heap->{'port'};
  if ($heap->{localaddr} and $heap->{localport}) {
    $heap->{localaddr} .= ":" . $heap->{localport};
  }
  
  # Disconnect if we're already logged into a server.
  if ($heap->{'sock'}) {
    $kernel->call( $session, 'quit' );
  }
  
  $heap->{'socketfactory'} =
    POE::Wheel::SocketFactory->new( SocketDomain   => AF_INET,
				    SocketType     => SOCK_STREAM,
				    SocketProtocol => 'tcp',
				    RemoteAddress  => $heap->{'server'},
				    RemotePort     => $heap->{'port'},
				    SuccessState   => '_sock_up',
				    FailureState   => '_sock_failed',
				    ($heap->{localaddr} ?
				     (BindAddress => $heap->{localaddr}) : ()),
				  );
}


# Send a CTCP query or reply, with the same syntax as a PRIVMSG event.
sub ctcp {
  my ($kernel, $state, $heap, $to) = @_[KERNEL, STATE, HEAP, ARG0];
  my $message = join '', @_[ARG1 .. $#_];
  
  unless (defined $to and defined $message) {
    die "The POE::Component::IRC event \"$state\" requires two arguments";
  }
  
  # CTCP-quote the message text.
  ($message) = @{$heap->{ctcp_filter}->put([ $message ])};
  
  # Should we send this as a CTCP request or reply?
  $state = $state eq 'ctcpreply' ? 'notice' : 'privmsg';
  
  $kernel->yield( $state, $to, $message );
}


# Attempt to initiate a DCC SEND or CHAT connection with another person.
sub dcc {
  my ($kernel, $heap, $nick, $type, $file, $blocksize) =
    @_[KERNEL, HEAP, ARG0 .. ARG3];
  my ($sock, $port, $myaddr, $size);
  
  unless ($type) {
    die "The POE::Component::IRC event \"dcc\" requires two arguments";
  }
  
  $type = uc $type;
  if ($type eq 'CHAT') {
    $file = 'chat';		# As per the semi-specification
    
  } elsif ($type eq 'SEND') {
    unless ($file) {
      die "The POE::Component::IRC event \"dcc\" requires three arguments for a SEND";
    }
    $size = (stat $file)[7];
    unless (defined $size) {
      _send_event( $kernel, $heap, 'irc_dcc_error',
		   "couldn't get ${file}'s size: $!", );
    }
  }
  
  # We can't use SocketFactory here because we need to know the port
  # number of the dynamically-assigned listening socket.
  $sock = gensym();
  socket $sock, PF_INET, SOCK_STREAM, getprotobyname('tcp')
    or die "Can't create listening DCC socket: $!";
  setsockopt $sock, SOL_SOCKET, SO_REUSEADDR, pack("l", 1)
    or die "Can't set SO_REUSEADDR on DCC listening socket: $!";
  $_ = select $sock;   $| = 1;   select $_;
  
  if ($heap->{localaddr} and $heap->{localaddr} =~ tr/a-zA-Z.//) {
    $heap->{localaddr} = inet_aton( $heap->{localaddr} );
  }
  bind $sock, sockaddr_in(0, $heap->{localaddr} || INADDR_ANY)
    or die "Can't bind a DCC listening socket: $!";
  listen $sock, SOMAXCONN or die "Can't listen to DCC socket: $!";
  
  # getsockname()'s address parameter is zero for INADDR_ANY sockets.
  ($port) = unpack_sockaddr_in(getsockname($sock));
  die "getsockname() failed on listening DCC socket: $!" unless $port > 0;
  $myaddr = $heap->{localaddr} || inet_aton(hostname() || 'localhost');
  die "Can't determine our IP address! ($!)" unless $myaddr;
  $myaddr = unpack "N", $myaddr;
  
  # Ask the kernel to tell us of incoming connection requests.
  $kernel->select( $sock, '_dcc_listen_accept', undef, undef );
  
  # Tell the other end that we're waiting for them to connect.
  $kernel->yield( 'ctcp', $nick, "DCC $type $file $myaddr $port"
		  . ($size ? " $size" : "") );
  
  # Store the state for this connection.
  $heap->{dcc_listen}->{$sock} = { nick => $nick,
				   type => $type,
				   file => $file,
				   blocksize => ($blocksize || BLOCKSIZE),
				   size => $size,
				   done => 0,
				 };
}


# Automatically replies to a PING from the server. Do not confuse this
# with CTCP PINGs, which are a wholly different animal that evolved
# much later on the technological timeline.
sub irc_ping {
  my ($kernel, $arg) = @_[KERNEL, ARG0];
  
  $kernel->yield( 'sl', "PONG $arg" );
}


# The way /notify is implemented in IRC clients.
sub ison {
  my ($kernel, @nicks) = @_[KERNEL, ARG0 .. $#_];
  my $toriplusplus = "ISON";
  
  die "No nicknames passed to POE::Component::IRC::ison" unless @nicks;
  
  # We can pass as many nicks as we want, as long as it's shorter than
  # the maximum command length (510). If the list we get is too long,
  # w'll break it into multiple ISON commands.
  while (@nicks) {
    my $nick = shift @nicks;
    if (length($toriplusplus) + 1 + length($nick) >= 510) {
      $kernel->yield( 'sl', $toriplusplus );
      $toriplusplus = "ISON";
    }
    $toriplusplus .= " $nick";
  }
  $kernel->yield( 'sl', $toriplusplus );
}


# Tell the IRC server to forcibly remove a user from a channel.
sub kick {
  my ($kernel, $chan, $nick) = @_[KERNEL, ARG0, ARG1];
  my $message = join '', @_[ARG1 .. $#_];
  
  unless (defined $chan and defined $nick) {
    die "The POE::Component::IRC event \"kick\" requires at least two arguments";
  }
  
  $nick .= " :$message" if defined $message;
  $kernel->yield( 'sl', "KICK $chan $nick" );
}


# Set up a new IRC component. Doesn't actually create and return an object.
sub new {
  my ($package, $alias) = splice @_, 0, 2;
  
  unless ($alias) {
    croak "Not enough arguments to POE::Component::IRC::new()";
  }
  
  POE::Session->new( 'rehash'    => \&noargs,
		     'restart'   => \&noargs,
		     'quit'      => \&oneoptarg,
		     'version'   => \&oneoptarg,
		     'time'      => \&oneoptarg,
		     'trace'     => \&oneoptarg,
		     'admin'     => \&oneoptarg,
		     'info'      => \&oneoptarg,
		     'away'      => \&oneoptarg,
		     'users'     => \&oneoptarg,
		     'wallops'   => \&oneoptarg,
		     'motd'      => \&oneoptarg,
		     'who'       => \&oneoptarg,
		     'nick'      => \&onlyonearg,
		     'oper'      => \&onlytwoargs,
		     'invite'    => \&onlytwoargs,
		     'squit'     => \&onlytwoargs,
		     'kill'      => \&onlytwoargs,
		     'privmsg'   => \&privandnotice,
		     'notice'    => \&privandnotice,
		     'join'      => \&oneortwo,
		     'summon'    => \&oneortwo,
		     'sconnect'  => \&oneandtwoopt,
		     'whowas'    => \&oneandtwoopt,
		     'stats'     => \&spacesep,
		     'links'     => \&spacesep,
		     'mode'      => \&spacesep,
		     'part'      => \&commasep,
		     'names'     => \&commasep,
		     'whois'     => \&commasep,
		     'ctcp'      => \&ctcp,
		     'ctcpreply' => \&ctcp,
		     $package => [qw( _dcc_listen_accept
				      _dcc_read
				      _parseline
				      _sock_down
				      _sock_failed
				      _sock_up
				      _start
				      _stop
				      connect
				      dcc
				      irc_ping
				      ison
				      kick
				      register
				      sl
				      topic
				      unregister
				      userhost )],
		     [ $alias, [@_] ] );
}


# The handler for all IRC commands that take no arguments.
sub noargs {
  my ($kernel, $state, $arg) = @_[KERNEL, STATE, ARG0];
  
  if (defined $arg) {
    die "The POE::Component::IRC event \"$state\" takes no arguments";
  }
  $kernel->yield( 'sl', uc $state );
}


# The handler for commands that take one required and two optional arguments.
sub oneandtwoopt {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $arg = join '', @_[ARG0 .. $#_];
  
  $state = uc $state;
  if (defined $arg) {
    $arg = ':' . $arg if $arg =~ /\s/;
    $state .= " $arg";
  }
  $kernel->yield( 'sl', $state );
}


# The handler for commands that take at least one optional argument.
sub oneoptarg {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $arg = join '', @_[ARG0 .. $#_];
  
  $state = uc $state;
  if (defined $arg) {
    $arg = ':' . $arg if $arg =~ /\s/;
    $state .= " $arg";
  }
  $kernel->yield( 'sl', $state );
}


# The handler for commands which take one required and one optional argument.
sub oneortwo {
  my ($kernel, $state, $one) = @_[KERNEL, STATE, ARG0];
  my $two = join '', @_[ARG1 .. $#_];
  
  unless (defined $one) {
    die "The POE::Component::IRC event \"$state\" requires at least one argument";
  }
  
  $state = uc( $state ) . " $one";
  $state .= " $two" if defined $two;
  $kernel->yield( 'sl', $state );
}


# Handler for commands that take exactly one argument.
sub onlyonearg {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $arg = join '', @_[ARG0 .. $#_];
  
  unless (defined $arg) {
    die "The POE::Component::IRC event \"$state\" requires one argument";
  }
  
  $state = uc $state;
  $arg = ':' . $arg if $arg =~ /\s/;
  $state .= " $arg";
  $kernel->yield( 'sl', $state );
}


# Handler for commands that take exactly two arguments.
sub onlytwoargs {
  my ($kernel, $state, $one) = @_[KERNEL, STATE, ARG0];
  my ($two) = join '', @_[ARG1 .. $#_];
  
  unless (defined $one and defined $two) {
    die "The POE::Component::IRC event \"$state\" requires two arguments";
  }
  
  $state = uc $state;
  $two = ':' . $two if $two =~ /\s/;
  $state .= " $one $two";
  $kernel->yield( 'sl', $state );
}


# Handler for privmsg or notice events.
sub privandnotice {
  my ($kernel, $state, $to) = @_[KERNEL, STATE, ARG0];
  my $message = join '', @_[ARG1 .. $#_];
  
  unless (defined $to and defined $message) {
    die "The POE::Component::IRC event \"$state\" requires two arguments";
  }
  
  if (ref $to eq 'ARRAY') {
    $to = join ',', @$to;
  }
  
  $state = uc $state;
  $state .= " $to :$message";
  $kernel->yield( 'sl', $state );
}


# Ask P::C::IRC to send you certain events, listed in $evref.
sub register {
  my ($heap, $sender, @events) = @_[HEAP, SENDER, ARG0 .. $#_];
  
  die "Not enough arguments" unless @events;
  
  # FIXME: What "special" event names go here? (ie, "errors")
  # basic, dcc (implies ctcp), ctcp, oper ...what other categories?
  foreach (@events) {
    $_ = "irc_" . $_ unless /^_/;
    $heap->{events}->{$_}->{$sender} = $sender;
    $heap->{sessions}->{$sender}->{'ref'} = $sender;
    $heap->{sessions}->{$sender}->{refcnt}++;
  }
}


# Send a line of IRC output to the server.
sub sl {
  my $heap = $_[HEAP];
  my $arg = join '', @_[ARG0 .. $#_];
  
  return unless $heap->{'socket'};
  die "Not enough arguments" unless defined $arg;
  
  warn ">>> $arg\n" if $heap->{'debug'};
  $heap->{'socket'}->put( "$arg\n" );
}


# The handler for commands which have N arguments, separated by spaces.
sub spacesep {
  my ($kernel, $state) = @_[KERNEL, STATE];
  my $args = join ' ', @_[ARG0 .. $#_];
  
  $state = uc $state;
  $state .= " $args" if defined $args;
  $kernel->yield( 'sl', $state );
}


# Set or query the current topic on a channel.
sub topic {
  my ($kernel, $chan) = @_[KERNEL, ARG0];
  my $topic = join '', @_[ARG1 .. $#_];
  
  $chan .= " :$topic" if defined $topic;
  $kernel->yield( 'sl', "TOPIC $chan" );
}


# Ask P::C::IRC to stop sending you certain events, listed in $evref.
sub unregister {
  my ($heap, $sender, @events) = @_[HEAP, SENDER, ARG0 .. $#_];
  
  die "Not enough arguments" unless @events;
  
  foreach (@events) {
    delete $heap->{events}->{$_}->{$sender};
    $heap->{sessions}->{$sender}->{refcnt}--;
    delete $heap->{sessions}->{$sender}
    if $heap->{sessions}->{$sender}->{refcnt} <= 0;
  }
}


# Asks the IRC server for some random information about particular nicks.
sub userhost {
  my ($kernel, @nicks) = @_[KERNEL, ARG0 .. $#_];
  my @five;
  
  die "No nicknames passed to POE::Component::IRC::userhost" unless @nicks;
  
  # According to the RFC, you can only send 5 nicks at a time.
  while (@nicks) {
    $kernel->yield( 'sl', "USERHOST " . join(' ', splice(@nicks, 0, 5)) );
  }
}



1;
__END__

=head1 NAME

POE::Component::IRC - a fully event-driven IRC client module.

=head1 SYNOPSIS

  use POE::Component::IRC;

# Do this when you create your sessions. 'my client' is just a
# kernel alias to christen the new IRC connection with. (Returns
# only a true or false success flag, not an object.)
POE::Component::IRC->new('my client') or die "Oh noooo! $!";

# Do stuff like this from within your sessions. This line tells the
# connection named "my client" to send your session the following
# events when they happen.
$kernel->post('my client', 'register', qw(connected msg public cdcc cping));
# You can guess what this line does.
$kernel->post('my client', 'connect',
	      { Nick     => 'Boolahman',
		Server   => 'irc-w.primenet.com',
		Port     => 6669,
		Username => 'quetzal',
		Ircname  => 'Ask me about my colon!', } );

=head1 DESCRIPTION

POE::Component::IRC is a POE component (who'd have guessed?) which
acts as an easily controllable IRC client for your other POE
components and sessions. You create an IRC component and tell it what
events your session cares about and where to connect to, and it sends
back interesting IRC events when they happen. You make the client do
things by sending it events. That's all there is to it. Cool, no?

[Note that using this module requires some familiarity with the
details of the IRC protocol. I'd advise you to read up on the gory
details of RFC 1459
E<lt>http://cs-pub.bu.edu/pub/irc/support/rfc1459.txtE<gt> before you
get started. Keep the list of server numeric codes handy while you
program. Needless to say, you'll also need a good working knowledge of
POE, or this document will be of very little use to you.]

So you want to write a POE program with POE::Component::IRC? Listen
up. The short version is as follows: Create your session(s) and an
alias for a new POE::Component::IRC client. (Conceptually, it helps if
you think of them as little IRC clients.) In your session's _start
handler, send the IRC client a 'register' event to tell it which IRC
events you want to receive from it. Send it a 'connect' event at some
point to tell it to join the server, and it should start sending you
interesting events every once in a while. If you want to tell it to
perform an action, like joining a channel and saying something witty,
send it the appropriate events like so:

  $kernel->post( 'my client', 'join', '#perl' );
  $kernel->post( 'my client', 'privmsg', '#perl', 'Pull my finger!' );

The long version is the rest of this document.

=head1 METHODS

Well, OK, there's only actually one, so it's more like "METHOD".

=over

=item new

Takes one argument: a name (kernel alias) which this new connection
will be known by. B<WARNING:> This method, for all that it's named
"new" and called in an OO fashion, doesn't actually return an
object. It returns a true or false value which indicates if the new
session was created or not. If it returns false, check $! for the
POE::Session error code.

=back

=head1 INPUT

How to talk to your new IRC component... here's the events we'll accept.

=head2 Important Commands

=over

=item ctcp and ctcpreply

Sends a CTCP query or response to the nick(s) or channel(s) which you
specify. Takes 2 arguments: the nick or channel to send a message to
(use an array reference here to specify multiple recipients), and the
plain text of the message to send (the CTCP quoting will be handled
for you).

=item connect

Takes one argument: a hash reference of attributes for the new
connection (see the L<SYNOPSIS> section of this doc for an
example). This event tells the IRC client to connect to a
new/different server. If it has a connection already open, it'll close
it gracefully before reconnecting. Possible attributes for the new
connection are "Server", the server name; "Password", an optional
password for restricted servers; "Port", the remote port number,
"LocalAddr", which local IP address on a multihomed box to connect as;
"LocalPort", the local TCP port to open your socket on; "Nick", your
client's IRC nickname; "Username", your client's username; and
"Ircname", some cute comment or something. C<connect()> will supply
reasonable defaults for any of these attributes which are missing, so
don't feel obliged to write them all out.

=item dcc

Send a DCC SEND or CHAT request to another person. Takes at least two
arguments: the nickname of the person to send the request to and the
type of DCC request (SEND or CHAT). For SEND requests, be sure to add
a third argument for the filename you want to send. Optionally, you
can add a fourth argument for the DCC transfer blocksize, but the
default of 1024 should usually be fine.

Incidentally, you can send other weird nonstandard kinds of DCCs too;
just put something besides 'SEND' or 'CHAT' (say, "FOO") in the type
field, and you'll get back "irc_dcc_foo" events when activity happens
on its DCC connection.

=item dcc_accept

In theory, this should accept an incoming DCC connection from another
person. In practice, it hasn't been implemented yet. Sorry!

=item join

Tells your IRC client to join a single channel of your choice. Takes
at least one arg: the channel name (required) and the channel key
(optional, for password-protected channels).

=item kick

Tell the IRC server to forcibly evict a user from a particular
channel. Takes at least 2 arguments: a channel name, the nick of the
user to boot, and an optional witty message to show them as they sail
out the door.

=item mode

Request a mode change on a particular channel or user. Takes at least
one argument: the mode changes to effect, as a single string (e.g.,
"+sm-p+o"), and any number of optional operands to the mode changes
(nicks, hostmasks, channel keys, whatever.) Or just pass them all as
one big string and it'll still work, whatever. I regret that I haven't
the patience now to write a detailed explanation.

=item nick

Allows you to change your nickname. Takes exactly one argument: the
new username that you'd like to be known as.

=item notice

Sends a NOTICE message to the nick(s) or channel(s) which you
specify. Takes 2 arguments: the nick or channel to send a notice to
(use an array reference here to specify multiple recipients), and the
text of the notice to send.

=item part

Tell your IRC client to leave the channels which you pass to it. Takes
any number of arguments: channel names to depart from.

=item privmsg

Sends a public or private message to the nick(s) or channel(s) which
you specify. Takes 2 arguments: the nick or channel to send a message
to (use an array reference here to specify multiple recipients), and
the text of the message to send.

=item quit

Tells the IRC server to disconnect you. Takes one optional argument:
some clever, witty string that other users in your channels will see
as you leave. You can expect to get an C<irc_disconnect> event shortly
after sending this.

=item register

Takes N arguments: a list of event names that your session wants to
listen for, minus the "irc_" prefix. So, for instance, if you just
want a bot that keeps track of which people are on a channel, you'll
need to listen for JOINs, PARTs, QUITs, and KICKs to people on the
channel you're in. You'd tell POE::Component::IRC that you want those
events by saying this:

  $kernel->post( 'my client', 'register', qw(join part quit kick) );

Then, whenever people enter or leave a channel your bot is on
(forcibly or not), your session will receive events with names like
"irc_join", "irc_kick", etc., which you can use to update a list of
people on the channel.

=item unregister

Takes N arguments: a list of event names which you I<don't> want to
receive. If you've previously done a 'register' for a particular event
which you no longer care about, this event will tell the IRC
connection to stop sending them to you. (If you haven't, it just
ignores you. No big deal.)

=back

=head2 Not-So-Important Commands

=over

=item admin

Asks your server who your friendly neighborhood server administrators
are. If you prefer, you can pass it a server name to query, instead of
asking the server you're currently on.

=item away

When sent with an argument (a message describig where you went), the
server will note that you're now away from your machine or otherwise
preoccupied, and pass your message along to anyone who tries to
communicate with you. When sent without arguments, it tells the server
that you're back and paying attention.

=item info

Basically the same as the "version" command, except that the server is
permitted to return any information about itself that it thinks is
relevant. There's some nice, specific standards-writing for ya, eh?

=item invite

Invites another user onto an invite-only channel. Takes 2 arguments:
the nick of the user you wish to admit, and the name of the channel to
invite them to.

=item ison

Asks the IRC server which users out of a list of nicknames are
currently online. Takes any number of arguments: a list of nicknames
to query the IRC server about.

=item links

Asks the server for a list of servers connected to the IRC
network. Takes two optional arguments, which I'm too lazy to document
here, so all you would-be linklooker writers should probably go dig up
the RFC.

=item motd

Request the server's "Message of the Day", a document which typically
contains stuff like the server's acceptable use policy and admin
contact email addresses, et cetera. Normally you'll automatically
receive this when you log into a server, but if you want it again,
here's how to do it. If you'd like to get the MOTD for a server other
than the one you're logged into, pass it the server's hostname as an
argument; otherwise, no arguments.

=item names

Asks the server for a list of nicknames on particular channels. Takes
any number of arguments: names of channels to get lists of users
for. If called without any channel names, it'll tell you the nicks of
everyone on the IRC network. This is a really big list, so don't do
this much.

=item sl

Sends a raw line of text to the server. Takes one argument: a string
of a raw IRC command to send to the server. It is more optimal to use
the events this module supplies instead of writing raw IRC commands
yourself.

=item stats

Returns some information about a server. Kinda complicated and not
terribly commonly used, so look it up in the RFC if you're
curious. Takes as many arguments as you please.

=item time

Asks the server what time it thinks it is, which it will return in a
human-readable form. Takes one optional argument: a server name to
query. If not supplied, defaults to current server.

=item topic

Retrieves or sets the topic for particular channel. If called with just
the channel name as an argument, it will ask the server to return the
current topic. If called with the channel name and a string, it will
set the channel topic to that string.

=item trace

If you pass a server name or nick along with this request, it asks the
server for the list of servers in between you and the thing you
mentioned. If sent with no arguments, it will show you all the servers
which are connected to your current server.

=item userhost

Asks the IRC server for information about particular nicknames. (The
RFC doesn't define exactly what this is supposed to return.) Takes any
number of arguments: the nicknames to look up.

=item users

Asks the server how many users are logged into it. Defaults to the
server you're currently logged into; however, you can pass a server
name as the first argument to query some other machine instead.

=item version

Asks the server about the version of ircd that it's running. Takes one
optional argument: a server name to query. If not supplied, defaults
to current server.

=item who

Lists the logged-on users matching a particular channel name, hostname,
nickname, or what-have-you. Takes one optional argument: a string for
it to search for. Wildcards are allowed; in the absence of this
argument, it will return everyone who's currently logged in (bad
move). Tack an "o" on the end if you want to list only IRCops, as per
the RFC.

=item whois

Queries the IRC server for detailed information about a particular
user. Takes any number of arguments: nicknames or hostmasks to ask for
information about.

=item whowas

Asks the server for information about nickname which is no longer
connected. Takes at least one argument: a nickname to look up (no
wildcards allowed), the optional maximum number of history entries to
return, and the optional server hostname to query.

=back

=head2 Purely Esoteric Commands

=over

=item oper

In the exceedingly unlikely event that you happen to be an IRC
operator, you can use this command to authenticate with your IRC
server. Takes 2 arguments: your username and your password.

=item rehash

Tells the IRC server you're connected to to rehash its configuration
files. Only useful for IRCops. Takes no arguments.

=item restart

Tells the IRC server you're connected to to shut down and restart itself.
Only useful for IRCops, thank goodness. Takes no arguments.

=item sconnect

Tells one IRC server (which you have operator status on) to connect to
another. This is actually the CONNECT command, but I already had an
event called 'connect', so too bad. Takes the args you'd expect: a
server to connect to, an optional port to connect on, and an optional
remote server to connect with, instead of the one you're currently on.

=item summon

Don't even ask.

=item wallops

Another opers-only command. This one sends a message to all currently
logged-on opers (and +w users); sort of a mass PA system for the IRC
server administrators. Takes one argument: some clever, witty message
to send.

=back

=head1 OUTPUT

The events you will receive (or can ask to receive) from your running
IRC component. Note that all incoming event names your session will
receive are prefixed by "irc_", to inhibit event namespace pollution.

If you wish, you can ask the client to send you every event it
generates. Simply register for the event name "all". This is a lot
easier than writing a huge list of things you specifically want to
listen for. FIXME: I'd really like to classify these somewhat
("basic", "oper", "ctcp", "dcc", "raw" or some such), and I'd welcome
suggestions for ways to make this easier on the user, if you can think
of some.

=head2 Important Events

=over

=item irc_connected

The IRC component will send an "irc_connected" event as soon as it
establishes a connection to an IRC server, before attempting to log
in. ARG0 is the server name.

B<NOTE:> When you get an "irc_connected" event, this doesn't mean you
can start sending commands to the server yet. Wait until you receive
an irc_001 event (the server welcome message) before actually sending
anything back to the server.

=item irc_ctcp_*

irc_ctcp_whatever events are generated upon receipt of CTCP messages.
For instance, a CTCP PING generates an irc_ctcp_ping event, CTCP SOURCE
generates an irc_ctcp_source event, blah blah, so on and so forth. ARG0
is the nick!hostmask of the sender. ARG1 is the channel/recipient
name(s). ARG2 is the text of the CTCP message.

=item irc_disconnected

The counterpart to irc_connected, sent whenever a socket connection
to an IRC server closes down (whether intentionally or
unintentionally). ARG0 is the server name.

=item irc_error

You get this whenever the server sends you an ERROR message. Expect
this to usually be accompanied by the sudden dropping of your
connection. ARG0 is the server's explanation of the error.

=item irc_join

Sent whenever someone joins a channel that you're on. ARG0 is the
person's nick!hostmask. ARG1 is the channel name.

=item irc_kick

Sent whenever someone gets booted off a channel that you're on. ARG0
is the kicker's nick!hostmask. ARG1 is the channel name. ARG2 is the
nick of the unfortunate kickee. ARG3 is the explanation string for the
kick.

=item irc_mode

Sent whenever someone changes a channel mode in your presence, or when
you change your own user mode. ARG0 is the nick!hostmask of that
someone. ARG1 is the channel it affects (or your nick, if it's a user
mode change). ARG2 is the mode string (i.e., "+o-b"). The rest of the
args (ARG3 .. $#_) are the operands to the mode string (nicks,
hostmasks, channel keys, whatever).

=item irc_msg

Sent whenever you receive a PRIVMSG command that was addressed to you
privately. ARG0 is the nick!hostmask of the sender. ARG1 is an array
reference containing the nick(s) of the recipients. ARG2 is the text
of the message.

=item irc_nick

Sent whenever you, or someone around you, changes nicks. ARG0 is the
nick!hostmask of the changer. ARG1 is the new nick that they changed
to.

=item irc_notice

Sent whenever you receive a NOTICE command. ARG0 is the nick!hostmask
of the sender. ARG1 is an array reference containing the nick(s) or
channel name(s) of the recipients. ARG2 is the text of the NOTICE
message.

=item irc_part

Sent whenever someone leaves a channel that you're on. ARG0 is the
person's nick!hostmask. ARG1 is the channel name.

=item irc_ping

An event sent whenever the server sends a PING query to the
client. (Don't confuse this with a CTCP PING, which is another beast
entirely. If unclear, read the RFC.) Note that POE::Component::IRC will
automatically take care of sending the PONG response back to the
server for you, although you can still register to catch the event for
informational purposes.

=item irc_public

Sent whenever you receive a PRIVMSG command that was sent to a
channel. ARG0 is the nick!hostmask of the sender. ARG1 is an array
reference containing the channel name(s) of the recipients. ARG2 is
the text of the message.

=item irc_quit

Sent whenever someone on a channel with you quits IRC (or gets
KILLed). ARG0 is the nick!hostmask of the person in question. ARG1 is
the clever, witty message they left behind on the way out.

=item irc_socketerr

Sent when a connection couldn't be established to the IRC server. ARG0
is probably some vague and/or misleading reason for what failed.

=item All numeric events (see RFC 1459)

Most messages from IRC servers are identified only by three-digit
numeric codes with undescriptive constant names like RPL_UMODEIS and
ERR_NOTOPLEVEL. (Actually, the list of codes in the RFC is kind of
out-of-date... the list in the back of Net::IRC::Event.pm is more
complete, and different IRC networks have different and incompatible
lists. Ack!) As an example, say you wanted to handle event 376
(RPL_ENDOFMOTD, which signals the end of the MOTD message). You'd
register for '376', and listen for 'irc_376' events. Simple, no? ARG0
is the name of the server which sent the message. ARG1 is the text of
the message.

=back

=head2 Somewhat Less Important Events

Note that receiving DCC connections isn't yet implemented. (That's why
this is a beta version of the module, after all. :-)

=over

=item irc_dcc_chat

Notifies you that one or more lines of text have been received from
the client on the other end of a DCC CHAT connection. ARG0 is the nick
of the person on the other end, ARG1 is the port number, and any
subsequent args are the individual text lines sent by the other
client.

=item irc_dcc_done

You receive this event when a DCC connection terminates
normally. Abnormal terminations are reported by "irc_dcc_error",
below. ARG0 is the nick of the person on the other end, ARG1 is the
DCC type (CHAT, SEND, GET, etc.), and ARG2 is the port number. For DCC
SEND and GET connections, ARG3 will be the filename, ARG4 will be the
file size, and ARG5 will be the number of bytes transferred. (ARG4 and
ARG5 should always be the same.)

=item irc_dcc_error

You get this event whenever a DCC connection or connection attempt
terminates unexpectedly or suffers some fatal error. ARG0 will be a
string describing the error. ARG1 will be the nick of the person on
the other end of the connection. ARG2 is the DCC type (SEND, GET,
CHAT, etc.). ARG4 is the port number of the DCC connection, if
any. For SEND and GET connections, ARG5 is the filename.

=item irc_dcc_send

Notifies you that another block of data has been successfully
transferred from you to the client on the other end of a DCC SEND
connection. ARG0 is the nick of the person on the other end, ARG1 is
the port number, ARG2 is the filename, ARG3 is the total file size, and
ARG4 is the number of bytes successfully transferred so far.

=item irc_dcc_start

This event notifies you that a DCC connection has been successfully
established. ARG0 is the nick of the person on the other end, ARG1 is
the DCC type (CHAT, SEND, GET, etc.), and ARG2 is the port
number. Note that the port number is a good way to uniquely identify a
particular DCC connection. For DCC SEND and GET connections, ARG3 will
be the filename and ARG4 will be the file size.

=item irc_snotice

A weird, non-RFC-compliant message from an IRC server. Don't worry
about it. ARG0 is the text of the server's message.

=back

=head1 AUTHOR

Dennis Taylor, E<lt>dennis@funkplanet.comE<gt>

=head1 MAD PROPS

The maddest of mad props go out to Rocco "dngor" Caputo
E<lt>troc@netrus.netE<gt>, for inventing something as mind-bogglingly
cool as POE, and to Kevin "oznoid" Lenzo E<lt>lenzo@cs.cmu.eduE<gt>,
for being the attentive parent of our precocious little infobot on
#perl.

Further props to a few of the studly bughunters who made this module not
suck: Abys <abys@web1-2-3.com>, Addi <addi@umich.edu>, ResDev
<ben@reser.org>, and Roderick <roderick@argon.org>. Woohoo!

=head1 SEE ALSO

Net::IRC, RFC 1459, http://www.irchelp.org/,
http://www.newts.org/~troc/poe.html, http://www.cs.cmu.edu/~lenzo/perl/,
http://www.infobot.org/,
http://newyork.citysearch.com/profile?fid=2&id=7104760,
http://www.pobox.com/~schwern/img/fishpants.jpg


=cut
