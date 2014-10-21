use strict;
use warnings;
use Test::More tests => 3;
use POE;
use POE::Component::IRC;
use POE::Component::IRC::Plugin::NickServID;

my $bot = POE::Component::IRC->spawn( plugin_debug => 1 );

POE::Session->create(
    package_states => [
        main => [ qw(_start irc_plugin_add irc_plugin_del) ],
    ],
);

$poe_kernel->run();

sub _start {
    $bot->yield(register => 'all');

    my $plugin = POE::Component::IRC::Plugin::NickServID->new( Password => 'test' );
    isa_ok($plugin, 'POE::Component::IRC::Plugin::NickServID');
  
    if (!$bot->plugin_add('TestPlugin', $plugin )) {
        fail('plugin_add failed');
        $bot->yield('shutdown');
    }
}

sub irc_plugin_add {
    my ($name, $plugin) = @_[ARG0, ARG1];
    return if $name ne 'TestPlugin';

    isa_ok($plugin, 'POE::Component::IRC::Plugin::NickServID');
  
    if (!$bot->plugin_del('TestPlugin') ) {
        fail('plugin_del failed');
        $bot->yield('shutdown');
    }
}

sub irc_plugin_del {
    my ($name, $plugin) = @_[ARG0, ARG1];
    return if $name ne 'TestPlugin';

    isa_ok($plugin, 'POE::Component::IRC::Plugin::NickServID');
    $bot->yield('shutdown');
}
