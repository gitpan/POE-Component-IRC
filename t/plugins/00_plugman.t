# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 9;
BEGIN { use_ok('POE::Component::IRC::State') };
BEGIN { use_ok('POE::Component::IRC::Plugin::PlugMan') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use POE;

my $self = POE::Component::IRC::State->spawn( );

isa_ok ( $self, 'POE::Component::IRC::State' );

POE::Session->create(
	inline_states => { _start => \&test_start, },
	package_states => [
	  'main' => [ qw(irc_plugin_add irc_plugin_del) ],
	],
	options => { trace => 0 },
);

$poe_kernel->run();
exit 0;

sub test_start {
  my ($kernel,$heap) = @_[KERNEL,HEAP];

  $self->yield( 'register' => 'all' );

  my $plugin = POE::Component::IRC::Plugin::PlugMan->new( debug => 0 );
  isa_ok ( $plugin, 'POE::Component::IRC::Plugin::PlugMan' );
  
  unless ( $self->plugin_add( 'TestPlugin' => $plugin ) ) {
	fail( 'plugin_add' );
  	$self->yield( 'unregister' => 'all' );
  	$self->yield( 'shutdown' );
  }

  undef;
}

sub irc_plugin_add {
  my ($kernel,$heap,$desc,$plugin) = @_[KERNEL,HEAP,ARG0,ARG1];
  return unless $desc eq "TestPlugin";

  isa_ok ( $plugin, 'POE::Component::IRC::Plugin::PlugMan' );

  ok( $plugin->load( 'Test1', 'POE::Component::IRC::Test::Plugin' ), "PlugMan_load" );
  ok( $plugin->reload( 'Test1' ), "PlugMan_reload" );
  ok( $plugin->unload( 'Test1' ), "PlugMan_unload" );
  
  unless ( $self->plugin_del( 'TestPlugin' ) ) {
  	fail( 'plugin_del' );
  	$self->yield( 'unregister' => 'all' );
  	$self->yield( 'shutdown' );
  }
  undef;
}

sub irc_plugin_del {
  my ($kernel,$heap,$desc,$plugin) = @_[KERNEL,HEAP,ARG0,ARG1];
  return unless $desc eq "TestPlugin";

  isa_ok ( $plugin, 'POE::Component::IRC::Plugin::PlugMan' );
  
  $self->yield( 'unregister' => 'all' );
  $self->yield( 'shutdown' );
  undef;
}
