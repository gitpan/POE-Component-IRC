# This make sures that we can close a DCC connection right after sending
# some data over it. The original bug was that the DCC plugin didn't post
# a delayed close event correctly so it ended up checking if there was data
# left to be sent on an undefined value rather than the wheel in question.

use strict;
use warnings;
use lib 't/inc';
use POE::Component::IRC;
use POE::Component::Server::IRC;
use POE qw(Wheel::SocketFactory);
use Socket;
use Test::More tests => 12;

my $bot1 = POE::Component::IRC->spawn(
    Flood        => 1,
    plugin_debug => 1,
);
my $bot2 = POE::Component::IRC->spawn(
    Flood        => 1,
    plugin_debug => 1,
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
            irc_disconnected
            irc_dcc_request
            irc_dcc_done
            irc_dcc_chat
            irc_dcc_start
        )],
    ],
);

$poe_kernel->run();

sub _start {
    my ($kernel, $heap) = @_[KERNEL, HEAP];

    my $wheel = POE::Wheel::SocketFactory->new(
        BindAddress  => '127.0.0.1',
        BindPort     => 0,
        SuccessEvent => '_fake_success',
        FailureEvent => '_fake_failure',
    );

    if ($wheel) {
        my $port = ( unpack_sockaddr_in( $wheel->getsockname ) )[0];
        $kernel->yield(_config_ircd => $port);
        $kernel->delay(_shutdown => 60);
        return;
    }

    $kernel->yield('_shutdown');
}

sub _config_ircd {
    my ($kernel, $port) = @_[KERNEL, ARG0];
    
    $ircd->yield(add_listener => Port => $port);
    
    $bot1->yield(register => 'all');
    $bot1->yield(connect => {
        nick    => 'TestBot1',
        server  => '127.0.0.1',
        port    => $port,
        ircname => 'Test test bot',
    });
  
    $bot2->yield(register => 'all');
    $bot2->yield(connect => {
        nick    => 'TestBot2',
        server  => '127.0.0.1',
        port    => $port,
        ircname => 'Test test bot',
    });
}

sub irc_001 {
    my $irc = $_[SENDER]->get_heap();
    pass('Logged in');
    $irc->yield(join => '#testchannel');
}

sub irc_join {
    my ($sender, $who, $where) = @_[SENDER, ARG0, ARG1];
    my $nick = ( split /!/, $who )[0];
    my $irc = $sender->get_heap();
    
    if ($nick eq $irc->nick_name()) {
        is($where, '#testchannel', 'Joined Channel Test');

        if ($nick eq 'TestBot2') {
            $irc->yield(dcc => TestBot1 => CHAT => '' => '' => 5);
        }
    }
}

sub irc_dcc_request {
    my ($sender, $cookie) = @_[SENDER, ARG3];
    pass('Got dcc request');
    $sender->get_heap()->yield(dcc_accept => $cookie);
}

sub irc_dcc_start {
    my ($sender, $id) = @_[SENDER, ARG0];
    my $irc = $sender->get_heap();
    pass('DCC started');

    if ($irc->nick_name() eq 'TestBot2') {
        $irc->yield(dcc_chat => $id => 'MOO');
        $irc->yield(dcc_close => $id);
    }
}

sub irc_dcc_chat {
    my ($sender, $what) = @_[SENDER, ARG3];
    is($what, 'MOO', 'DCC CHAT test');
}

sub irc_dcc_done {
    pass('Got dcc close');
    $_[SENDER]->get_heap()->yield('quit');
}

sub irc_disconnected {
    my ($kernel, $heap) = @_[KERNEL, HEAP];
    pass('irc_disconnected');
    $heap->{count}++;
    $kernel->yield('_shutdown') if $heap->{count} == 2;
}

sub _shutdown {
    my ($kernel) = $_[KERNEL];

    $kernel->alarm_remove_all();
    $ircd->yield('shutdown'); 
    $bot1->yield('shutdown');
    $bot2->yield('shutdown');
}
