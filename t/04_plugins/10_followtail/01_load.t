use strict;
use warnings;
use POE qw(Filter::Line);
use POE::Component::IRC;
use POE::Component::IRC::Plugin::FollowTail;
use Test::More;

my $fh;
eval {
    open $fh, '>', 'followtail' or die "$!\n";
    $fh->autoflush(1);
    print $fh "moocow\n";
};

if ($@) {
    plan skip_all => "Couldn't create a file and write to it";
}

plan tests => 5;
my $bot = POE::Component::IRC->spawn( plugin_debug => 1 );

POE::Session->create(
    package_states => [
        main => [ qw(_start irc_plugin_add irc_plugin_del irc_tail_input) ],
    ],
);

$poe_kernel->run();

sub _start {
    $bot->yield(register => 'all');

    my $plugin = POE::Component::IRC::Plugin::FollowTail->new( 
        filename => 'followtail',
        filter   => POE::Filter::Line->new(),
    );
    
    isa_ok($plugin, 'POE::Component::IRC::Plugin::FollowTail');
  
    if (!$bot->plugin_add('TestPlugin', $plugin) ) {
        fail('plugin_add failed');
        $bot->yield('shutdown');
    }
}

sub irc_plugin_add {
    my ($name, $plugin) = @_[ARG0, ARG1];
    return if $name ne 'TestPlugin';
    
    isa_ok($plugin, 'POE::Component::IRC::Plugin::FollowTail');
    print $fh "Cows go moo, yes they do\n";
}

sub irc_tail_input {
    my ($sender, $filename, $input) = @_[SENDER, ARG0, ARG1];
    my $irc = $sender->get_heap();
    
    is($filename, 'followtail', 'Filename is okay');
    is($input, 'Cows go moo, yes they do', 'Cows go moo!');

    if (!$irc->plugin_del('TestPlugin')) {
        fail('plugin_del failed');
        $irc->yield('shutdown');
    }
}

sub irc_plugin_del {
    my ($sender, $name, $plugin) = @_[SENDER, ARG0, ARG1];
    my $irc = $sender->get_heap();
    return if $name ne 'TestPlugin';

    isa_ok($plugin, 'POE::Component::IRC::Plugin::FollowTail'); 
    $irc->yield('shutdown');
}