use strict;
use warnings;
use lib 't/inc';
use File::Spec::Functions qw(catfile);
use POE qw(Wheel::SocketFactory);
use POE::Component::IRC::State;
use POE::Component::IRC::Plugin::Logger;
use POE::Component::Server::IRC;
use Socket;
use Test::More;

my $bot1 = POE::Component::IRC::State->spawn(
    Flood        => 1,
    plugin_debug => 1,
);
my $bot2 = POE::Component::IRC::State->spawn(
    Flood        => 1,
    plugin_debug => 1,
);
my $ircd = POE::Component::Server::IRC->spawn(
    Auth      => 0,
    AntiFlood => 0,
);

$bot2->plugin_add(Logger => POE::Component::IRC::Plugin::Logger->new(
    Path => 'logger_test',
    Notices => 1,
));

my $file = catfile('logger_test', 'testbot1.log');
unlink $file if -e $file;

my @correct = (
    '<TestBot1> Hello there',
    '<TestBot2> Hi yourself',
    '* TestBot1 is talking',
    '* TestBot2 is too',
    '>TestBot1< This is a notice',
    '>TestBot2< So is this',
);

plan tests => 8 + @correct;

POE::Session->create(
    package_states => [
        main => [qw(
            _start
            _config_ircd
            _shutdown
            irc_001
            irc_msg
            irc_ctcp_action
            irc_notice
            irc_disconnected
        )],
    ],
);

$poe_kernel->run();

sub _start {
    my ($kernel) = $_[KERNEL];

    my $ircd_port = get_port() or $kernel->yield(_shutdown => 'No free port');
    $kernel->yield(_config_ircd => $ircd_port);
    $kernel->delay(_shutdown => 60, 'Timed out');
}

sub get_port {
    my $wheel = POE::Wheel::SocketFactory->new(
        BindAddress  => '127.0.0.1',
        BindPort     => 0,
        SuccessEvent => '_fake_success',
        FailureEvent => '_fake_failure',
    );

    return if !$wheel;
    return unpack_sockaddr_in($wheel->getsockname()) if wantarray;
    return (unpack_sockaddr_in($wheel->getsockname))[0];
}

sub _shutdown {
    my ($kernel, $error) = @_[KERNEL, ARG0];
    fail($error) if defined $error;
    
    $kernel->alarm_remove_all();
    $ircd->yield('shutdown');
    $bot1->yield('shutdown');
    $bot2->yield('shutdown');
}

sub _config_ircd {
    my ($kernel, $port) = @_[KERNEL, ARG0];
    
    $ircd->yield(add_listener => Port => $port);
    
    $bot1->yield(register => 'all');
    $bot1->yield(connect => {
        nick    => 'TestBot1',
        server  => '127.0.0.1',
        port    => $port,
    });
    
    $bot2->yield(register => 'all');
    $bot2->yield(connect => {
        nick    => 'TestBot2',
        server  => '127.0.0.1',
        port    => $port,
    });
}

sub irc_001 {
    my ($heap, $server) = @_[HEAP, ARG0];
    my $irc = $_[SENDER]->get_heap();
    
    pass($irc->nick_name() . ' logged in');

    if ($irc == $bot2) {
        $bot1->yield(privmsg => $bot2->nick_name(), 'Hello there');
        $heap->{msg}++;
    }
}

sub irc_msg {
    my $heap = $_[HEAP];

    pass('irc_msg');
    if ($heap->{msg} == 1) {
        $bot2->yield(privmsg => $bot1->nick_name(), 'Hi yourself');
        $heap->{msg}++;
    }
    elsif ($heap->{msg} == 2) {
        $bot1->yield(ctcp => $bot2->nick_name(), 'ACTION is talking');
        $heap->{msg}++;
    }
}

sub irc_ctcp_action {
    my $heap = $_[HEAP];

    pass('irc_ctcp_action');
    if ($heap->{msg} == 3) {
        $bot2->yield(ctcp => $bot1->nick_name(), 'ACTION is too');
        $heap->{msg}++;
    }
    elsif ($heap->{msg} == 4) {
        $bot1->yield(notice => $bot2->nick_name(), 'This is a notice');
        $heap->{msg}++;
    }
}

sub irc_notice {
    my $heap = $_[HEAP];

    if ($heap->{msg} == 5) {
        $bot2->yield(notice => $bot1->nick_name(), 'So is this');
        $heap->{msg}++;
    }
    elsif ($heap->{msg} == 6) {
        $bot1->yield('quit');
        $bot2->yield('quit');
    }
}

sub irc_disconnected {
    my ($kernel, $heap) = @_[KERNEL, HEAP];
    pass('irc_disconnected');
    $heap->{count}++;

    if ($heap->{count} == 2) {
        verify_log();
        $kernel->yield('_shutdown');
    }
}

sub verify_log {
    open my $log, '<', $file or die "Can't open log file '$file': $!";
    my @lines = <$log>;
    close $log;

    my $check = 0;
    for my $line (@lines) {
        next if $line =~ /^\*{3}/;
        chomp $line;
        $line = substr($line, 20);
        last if !defined $correct[$check];

        if (ref $correct[$check] eq 'Regexp') {
            like($line, $correct[$check], 'Line ' . ($check+1));
        }
        else {
            is($line, $correct[$check], 'Line ' . ($check+1));
        }
        $check++;
    }

    fail('Log too short') if $check > @correct;
}
