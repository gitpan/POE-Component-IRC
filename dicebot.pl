#!/usr/bin/perl -w

use strict;
use POE::Kernel;
use POE::Session;
use POE::Component::IRC;

my $nick = 'dicebot';


sub _start {
  my ($kernel) = $_[KERNEL];

  $kernel->alias_set( 'smileyninja' );
  $kernel->post( 'dicebot', 'register', 'all');
  $kernel->post( 'dicebot', 'connect', { Debug    => 0,
					 Nick     => $nick,
					 Server   => $ARGV[0] ||
					             'binky.rhizomatic.net',
					 Port     => $ARGV[1] || 6667,
					 Username => 'neenio',
					 Ircname  => "HELP I'M A ROCK", }
	       );
}

sub irc_001 {
  my ($kernel) = $_[KERNEL];

  $kernel->post( 'dicebot', 'mode', $nick, '+i' );
  $kernel->post( 'dicebot', 'join', '#dice' );
  $kernel->post( 'dicebot', 'privmsg', '#dice', 'I am a dice-rolling bot.' );
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

sub _stop {
  my ($kernel) = $_[KERNEL];

  print "Control session stopped.\n";
  $kernel->post( 'dicebot', 'quit', 'Neenios on ice!' );
  $kernel->alias_remove( 'smileyninja' );
}

sub irc_public {
  my ($kernel, $who, $chan, $msg) = @_[KERNEL, ARG0 .. ARG2];
  $who =~ s/^(.*)!.*$/$1/ or die "Weird-ass who: $who";

  my ($num, $die, $plus) =
    ($msg =~ /^\s*(?:$nick,?\s*)?roll (?:a )?(\d+)d(\d+)\s*([+-]\s*(\d+))?/i);
  return unless $num and $die and $num < 100 and $die < 10000;
  $plus =~ tr/+ //d if $plus;  # yes, this is what I meant!

  my @rolls;
  my $sum = $plus || 0;
  for (1 .. $num) {
    push @rolls, int rand( $die ) + 1;
    $sum += $rolls[-1];
  }

  my $str = "You rolled a $sum";
  if (@rolls > 1 or $plus) {
    $plus = '' unless $plus;
    $plus =~ s/\-(\d)/ \- $1/;
    $plus =~ s/^(\d)/ \+ $1/;
    $str .= " (" . join( " + ", @rolls) . "$plus)";
  }

  $kernel->post( 'dicebot', 'privmsg', $chan, "$who: $str" );
}


POE::Component::IRC->new( 'dicebot' ) or
  die "Can't instantiate new IRC component!\n";
POE::Session->new( 'main' => [qw(_start _stop irc_001 irc_disconnected
                                 irc_socketerr irc_error irc_public)] );
$poe_kernel->run();

exit 0;
