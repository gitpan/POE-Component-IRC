#!/usr/bin/perl -w
#
# $Id: test.pl,v 1.9 1999/12/12 11:48:07 dennis Exp $
#
# This simple test program should give you an idea of how a basic
# POE::Component::IRC script fits together.
# -- dennis taylor, <dennis@funkplanet.com>

use strict;
use POE::Kernel;
use POE::Session;
use POE::Component::IRC;

my $nick = "spleen" . ($$ % 1000);


# This gets executed as soon as the kernel sets up this session.
sub _start {
  my ($kernel, $session) = @_[KERNEL, SESSION];

  # Uncomment this to turn on more verbose POE debugging information.
  # $session->option( trace => 1 );

  # Make an alias for our session, to keep it from getting GC'ed.
  $kernel->alias_set( 'smileyninja' );

  # Ask the IRC component to send us all IRC events it receives. This
  # is the easy, indiscriminate way to do it.
  $kernel->post( 'test', 'register', 'all');

  # Setting Debug to 1 causes P::C::IRC to print all raw lines of text
  # sent to and received from the IRC server. Very useful for debugging.
  $kernel->post( 'test', 'connect', { Debug    => 0,
				      Nick     => $nick,
				      Server   => 'irc.metronomicon.com',
				      Port     => 6668,
				      Username => 'neenio',
				      Ircname  => 'Ask me about my colon!', }
	       );
}


# After we successfully log into the IRC server, join a channel.
sub irc_001 {
  my ($kernel) = $_[KERNEL];

  $kernel->post( 'test', 'mode', $nick, '+i' );
  $kernel->post( 'test', 'join', '#IRC.pm' );
  $kernel->post( 'test', 'away', 'JOSHUA SCHACTER IS MY SLIPPERY TURGID ZUCCHINI OF LUST' );
}


sub irc_dcc_send {
  my ($nick, $port, $file, $size, $done) = @_[ARG0 .. $#_];

  printf "DCC SEND to $nick ($file): $done bytes of $size sent.   %d%%\n",
	($done / $size) * 100;
}


sub _default {
  my ($state, $event, $args) = @_[STATE, ARG0, ARG1];

  $args ||= [];
  print "$state -- $event @$args\n";
}


sub _stop {
  my ($kernel) = $_[KERNEL];

  print "Control session stopped.\n";
  $kernel->post( 'test', 'quit', 'Neenios on ice!' );
  $kernel->alias_remove( 'smileyninja' );
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


sub irc_kick {
  my ($who, $where, $isitme, $reason) = @_[ARG0 .. ARG4];

  print "Kicked from $where by $who: $reason\n" if $isitme eq $nick;
}


# here's where execution starts.

# Change this '0' to '1' for lots of debugging information.
POE::Component::IRC->new( 'test' ) or
  die "Can't instantiate new IRC component!\n";
POE::Session->new( 'main' => [qw(_start _stop _default irc_001 irc_kick
				 irc_disconnected irc_error irc_socketerr
				 irc_dcc_send)] );
$poe_kernel->run();

exit 0;
