use strict;
use warnings;
use lib 't/inc';
use POE qw(Wheel::SocketFactory);
use Socket;
use POE::Component::IRC::State;
use POE::Component::IRC::Plugin::AutoJoin;
use POE::Component::Server::IRC;
use Test::More tests => 10;

my $bot1 = POE::Component::IRC::State->spawn(
    Flood        => 1,
    plugin_debug => 1,
);
my $bot2 = POE::Component::IRC::State->spawn(
    Flood        => 1,
    plugin_debug => 1,
    AwayPoll     => 1,
);
my $ircd = POE::Component::Server::IRC->spawn(
    Auth      => 0,
    AntiFlood => 0,
);

POE::Session->create(
    package_states => [
        main => [qw(
            _start
            _config_ircd 
            _shutdown 
            irc_001 
            irc_join
            irc_chan_sync
            irc_user_away
            irc_user_back
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

    return (unpack_sockaddr_in($wheel->getsockname))[0] if $wheel;
    return;
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
    my $irc = $_[SENDER]->get_heap();
    pass($irc->nick_name . ' logged in');
    
    if ($irc == $bot1) {
        $irc->yield(join => '#testchannel');
    }
}

sub irc_join {
    my ($sender, $who, $where) = @_[SENDER, ARG0, ARG1];
    my $nick = ( split /!/, $who )[0];
    my $irc = $sender->get_heap();
    
    return if $nick ne $irc->nick_name();
    is($where, '#testchannel', $irc->nick_name . ' joined channel');
}

sub irc_chan_sync {
    my ($sender, $where) = @_[SENDER, ARG0];
    my $irc = $sender->get_heap();

    is($where, '#testchannel', $irc->nick_name . ' synced channel');

    if ($irc == $bot1) {
        $bot2->yield(join => $where);
    }
    else {
        $bot1->yield(away => "I'm gone now");
        $bot2->yield(away => "I'm gone now");
    }
}

sub irc_user_away {
    my ($sender, $nick) = @_[SENDER, ARG0];
    my $irc = $sender->get_heap();

    if ($irc == $bot1) {
        fail("Shouldn't get irc_user_away when AwayPoll is off");
    }

    is($nick, $bot1->nick_name(), $bot1->nick_name() .' went away');
    $bot1->yield('away');
    $bot2->yield('away');
}

sub irc_user_back {
    my ($sender, $nick) = @_[SENDER, ARG0];
    my $irc = $sender->get_heap();

    if ($irc == $bot1) {
        fail("Shouldn't get irc_user_back when AwayPoll is off");
    }
    
    is($nick, $bot1->nick_name(), $bot1->nick_name() .' came back');
    $_->yield('quit') for ($bot1, $bot2);
}

sub irc_disconnected {
    my ($kernel, $heap, $info) = @_[KERNEL, HEAP, ARG1];
    pass($info->{Nick} . ' disconnected');
    $heap->{count}++;
    $kernel->yield('_shutdown') if $heap->{count} == 2;
}

sub _shutdown {
    my ($kernel, $error) = @_[KERNEL, ARG0];
    fail($error) if defined $error;
    
    $kernel->alarm_remove_all();
    $ircd->yield('shutdown');
    $bot1->yield('shutdown');
    $bot2->yield('shutdown');
}

