#!/usr/bin/perl -w
#
# This bot is a simple telnet proxy. You DCC CHAT with it, and it
# connects to somewhere else, and you talk to the somewhere else over
# the CHAT connection in your IRC client. I originally wrote it because
# I wanted to use XChat as an interface to a MOO instead of telnet. :-)
#
# All things considered, a good demonstration of DCC code.
#
# -- dennis taylor, <dennis@funkplanet.com>

use strict;
use Socket;
use POE qw( Wheel::SocketFactory Wheel::ReadWrite Filter::Line Driver::SysRW );
use POE::Component::IRC;

my $mynick = "moo";
my $user = "(fimm(tiu)?|(Half|Semi)jack|stimps)";
my $telnethost = "binky";
my $telnetport = 7788;

my $chatsession = undef;

sub _start {
  my ($kernel, $session) = @_[KERNEL, SESSION];

  # $session->option( trace => 1 );
  $kernel->alias_set( 'smileyninja' );
  $kernel->post( 'irc', 'register', 'all');
  $kernel->post( 'irc', 'connect', { Debug    => 0,
				     Nick     => $mynick,
				     Server   => 'binky',
				     Port     => 6667,
				     Username => 'neenio',
				     Ircname  => 'Ask me about my colon!', }
	       );
}


sub _connected {
  my ($kernel, $heap, $sock, $addr, $port) = @_[KERNEL, HEAP, ARG0 .. ARG2];

  $heap->{wheel} = POE::Wheel::ReadWrite->new(
      Handle => $sock,
      Filter => POE::Filter::Line->new(),
      Driver => POE::Driver::SysRW->new(),
      InputState => '_conn_data',
      ErrorState => '_conn_error',
  );

  $kernel->post( 'irc', 'dcc_chat', $chatsession, "*** Connected." );
  print "Connected.\n";
}


sub _connect_failed {
  my ($kernel, $heap, $function, $errstr) = @_[KERNEL, HEAP, ARG0, ARG2];

  $kernel->post( 'irc', 'dcc_chat', $chatsession,
		 "*** Couldn't connect to $telnethost:$telnetport: $errstr in $function" );
  print "Couldn't connect to $telnethost:$telnetport: $errstr in $function\n";
  delete $heap->{wheel};
}


sub _conn_data {
  my ($kernel, $heap, $line) = @_[KERNEL, HEAP, ARG0];

  $line = " " unless length $line;
  $kernel->post( 'irc', 'dcc_chat', $chatsession, $line );
  print "<== $line\n";
}


sub _conn_error {
  my ($kernel, $heap, $function, $errstr) = @_[KERNEL, HEAP, ARG0, ARG2];

  $kernel->post( 'irc', 'dcc_chat', $chatsession,
		 "*** Connection to $telnethost:$telnetport lost: $errstr in $function" );
  print "Connection to $telnethost:$telnetport lost: $errstr in $function\n";
  delete $heap->{wheel};
}


# After we successfully log into the IRC server, make ourselves invisible.
sub irc_001 {
  $_[KERNEL]->post( 'irc', 'mode', $mynick, '+i' );
}


sub irc_dcc_request {
  my ($kernel, $heap, $nick, $type, $port, $cookie) =
    @_[KERNEL, HEAP, ARG0 .. ARG3];

  $nick =~ s/^(.+?)!.*$/$1/;
  unless ($nick =~ /^$user$/o and $type eq "CHAT") {
    $kernel->post( 'irc', 'notice', $nick, "Buzz off." );
    return;
  }

  if ($port < 1024) {
    $kernel->post( 'irc', 'notice', $nick, "Reserved ports are beneath me." );
    return;
  }

  $kernel->post( 'irc', 'dcc_accept', $cookie );
}


sub irc_dcc_start {
  my ($kernel, $heap, $cookie, $nick, $port) =
    @_[KERNEL, HEAP, ARG0, ARG1, ARG3];

  unless ($chatsession) {
    die "Who the hell is \"$nick\"?" unless $nick =~ /^$user!.*$/o;
    print "DCC CHAT connection established with $nick on port $port.\n";
  }

  $chatsession = $cookie;   # save the magic cookie
  $kernel->post( 'irc', 'dcc_chat', $chatsession,
		 "*** Connecting to $telnethost, port $telnetport..." );

  $heap->{factory} = POE::Wheel::SocketFactory->new(
      RemoteAddress => $telnethost,
      RemotePort    => $telnetport,
      SuccessState  => '_connected',
      FailureState  => '_connect_failed',
  );
}


sub irc_dcc_chat {
  my ($kernel, $heap, $peer, $line) = @_[KERNEL, HEAP, ARG1, ARG3];

  if ($line eq "***reconnect" and not exists $heap->{wheel}) {
    $kernel->yield( 'irc_dcc_start', $chatsession, '', $peer, 0 );

  } elsif ($line eq "***quit") {
    $kernel->alias_remove( 'smileyninja' );
    delete $heap->{factory};
    delete $heap->{wheel};

  } else {
    if ($line =~ /^\001ACTION (.*)\001\015?$/) {
      $line = ":$1";
    }

    $heap->{wheel}->put( $line ) if exists $heap->{wheel};
    print "==> $line\n";
  }
}


sub irc_dcc_done {
  my ($nick, $type) = @_[ARG0, ARG1];
  print "DCC $type to $nick closed.\n",
  $chatsession = undef;
}


sub irc_dcc_error {
  my ($err, $nick, $type) = @_[ARG1 .. ARG3];
  print "DCC $type to $nick failed: $err.\n",
  $chatsession = undef;
}


sub _signal {
  my ($kernel, $heap) = @_[KERNEL, HEAP];

  $kernel->alias_remove( 'smileyninja' );
  delete $heap->{factory};
  delete $heap->{wheel};
}


sub _stop {
  my ($kernel) = $_[KERNEL];

  print "Control session stopped.\n";
  $kernel->post( 'irc', 'quit', 'Control session stopped.' );
}


sub irc_disconnected {
  my ($server) = $_[ARG0];
  print "Lost connection to server $server.\n";
}


sub irc_error {
  my $err = $_[ARG0];
  print "Server error occurred! $err\n";
}


sub irc_socketerr {
  my $err = $_[ARG0];
  print "Couldn't connect to server: $err\n";
}


POE::Component::IRC->new( 'irc', trace => undef ) or
  die "Can't instantiate new IRC component!\n";
POE::Session->new( 'main' => [qw( _start _stop _connected _signal
				  _connect_failed _conn_data _conn_error
				  irc_001 irc_error irc_disconnected
				  irc_socketerr irc_dcc_start irc_dcc_done
				  irc_dcc_chat irc_dcc_error irc_dcc_request)]
		 );
$poe_kernel->run();

exit 0;
