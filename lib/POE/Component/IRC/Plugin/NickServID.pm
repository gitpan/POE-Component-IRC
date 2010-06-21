package POE::Component::IRC::Plugin::NickServID;
BEGIN {
  $POE::Component::IRC::Plugin::NickServID::VERSION = '6.33';
}
BEGIN {
  $POE::Component::IRC::Plugin::NickServID::AUTHORITY = 'cpan:HINRIK';
}

use strict;
use warnings;
use Carp;
use POE::Component::IRC::Plugin qw( :ALL );
use POE::Component::IRC::Common qw( u_irc );

sub new {
    my ($package) = shift;
    croak "$package requires an even number of arguments" if @_ & 1;
    my %self = @_;
    
    die "$package requires a Password" if !defined $self{Password};
    return bless \%self, $package;
}

sub PCI_register {
    my ($self, $irc) = @_;
    $self->{nick} = $irc->{nick};
    $self->{irc} = $irc;
    $irc->plugin_register($self, 'SERVER', qw(004 nick));
    return 1;
}

sub PCI_unregister {
    return 1;
}

sub S_004 {
    my ($self, $irc) = splice @_, 0, 2;
    my $version = ${ $_[2] }->[1];

    $self->{ratbox} = $version =~ /ratbox/i ? 1 : 0;
    $self->_identify();
    return PCI_EAT_NONE;
}

sub S_nick {
    my ($self, $irc) = splice @_, 0, 2;
    my $mapping = $irc->isupport('CASEMAPPING');
    my $new_nick = u_irc( ${ $_[1] }, $mapping );

    if ( $new_nick eq u_irc($self->{nick}, $mapping) ) {
        $self->_identify();
    }
    return PCI_EAT_NONE;
}

sub _identify {
    my ($self) = @_;
    my $irc = $self->{irc};

    if ($self->{ratbox}) {
        $irc->yield(quote => "NS IDENTIFY $self->{Password}");
    }
    else {
        $irc->yield(nickserv => "IDENTIFY $self->{Password}");
    }
    return;
}

1;
__END__

=encoding utf8

=head1 NAME

POE::Component::IRC::Plugin::NickServID - A PoCo-IRC plugin
which identifies with FreeNode's NickServ when needed

=head1 SYNOPSIS

 use POE::Component::IRC::Plugin::NickServID;

 $irc->plugin_add( 'NickServID', POE::Component::IRC::Plugin::NickServID->new(
     Password => 'opensesame'
 ));

=head1 DESCRIPTION

POE::Component::IRC::Plugin::NickServID is a L<POE::Component::IRC|POE::Component::IRC>
plugin. It identifies with NickServ on connect and when you change your nick,
if your nickname matches the supplied password.

B<Note>: If you have a cloak and you don't want to be seen without it, make sure
you don't join channels until after you've identified yourself. If you use the
L<AutoJoin plugin|POE::Component::IRC::Plugin::AutoJoin>, it will be taken
care of for you.

=head1 METHODS

=head2 C<new>

Arguments:

'Password', the NickServ password.

Returns a plugin object suitable for feeding to
L<POE::Component::IRC|POE::Component::IRC>'s plugin_add() method.

=head1 AUTHOR

Hinrik E<Ouml>rn SigurE<eth>sson, hinrik.sig@gmail.com

=cut
