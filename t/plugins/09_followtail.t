# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 1.t'

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use Test::More tests => 8;
BEGIN { use_ok('POE::Component::IRC') };
BEGIN { use_ok('POE::Component::IRC::Plugin::FollowTail') };

#########################

# Insert your test code below, the Test::More module is use()ed here so read
# its man page ( perldoc Test::More ) for help writing this test script.

use POE qw(Filter::Line);

open FH, '> followtail' or die "$!\n";
FH->autoflush(1);
print FH "moocow\n";

my $self = POE::Component::IRC->spawn( );

isa_ok ( $self, 'POE::Component::IRC' );

POE::Session->create(
	inline_states => { _start => \&test_start, },
	package_states => [
	  'main' => [ qw(irc_plugin_add irc_plugin_del irc_tail_input) ],
	],
);

$poe_kernel->run();
exit 0;

sub test_start {
  my ($kernel,$heap) = @_[KERNEL,HEAP];

  $self->yield( 'register' => 'all' );

  my $plugin = POE::Component::IRC::Plugin::FollowTail->new( 
	filename => 'followtail',
	filter   => POE::Filter::Line->new(),
  );
  isa_ok ( $plugin, 'POE::Component::IRC::Plugin::FollowTail' );
  
  unless ( $self->plugin_add( 'TestPlugin' => $plugin ) ) {
	fail( 'plugin_add' );
  	$self->yield( 'unregister' => 'all' );
  	$self->yield( 'shutdown' );
  }

  undef;
}

sub irc_plugin_add {
  my ($kernel,$heap,$desc,$plugin) = @_[KERNEL,HEAP,ARG0,ARG1];

  isa_ok ( $plugin, 'POE::Component::IRC::Plugin::FollowTail' );
  
#  unless ( $self->plugin_del( 'TestPlugin' ) ) {
#  	fail( 'plugin_del' );
#  	$self->yield( 'unregister' => 'all' );
#  	$self->yield( 'shutdown' );
#  }
  print FH "Cows go moo, yes they do\n";
  undef;
}

sub irc_tail_input {
  my ($kernel,$filename,$input) = @_[KERNEL,ARG0,ARG1];
  close FH;
  ok( $filename eq 'followtail', 'Filename is okay' );
  ok( $input eq 'Cows go moo, yes they do', 'Cows go moo!' );
  unless ( $self->plugin_del( 'TestPlugin' ) ) {
  	fail( 'plugin_del' );
  	$self->yield( 'unregister' => 'all' );
  	$self->yield( 'shutdown' );
  }
  return;
}

sub irc_plugin_del {
  my ($kernel,$heap,$desc,$plugin) = @_[KERNEL,HEAP,ARG0,ARG1];

  isa_ok ( $plugin, 'POE::Component::IRC::Plugin::FollowTail' );
  
  $self->yield( 'unregister' => 'all' );
  $self->yield( 'shutdown' );
  undef;
}
