# $Id: Makefile.PL,v 3.6 2005/03/04 17:37:22 chris Exp $

use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
			  'NAME'	=> 'POE::Component::IRC',
			  'PM'      => { 'IRC.pm' => '$(INST_LIBDIR)/IRC.pm',
							 'IRC-State.pm' => '$(INST_LIBDIR)/IRC/State.pm',
							 'IRC-Qnet.pm' => '$(INST_LIBDIR)/IRC/Qnet.pm',
							 'IRC-Qnet-State.pm' => '$(INST_LIBDIR)/IRC/Qnet/State.pm',
							 'Filter-IRC.pm' => '$(INST_LIBDIR)/../Filter/IRC.pm',
							 'Filter-CTCP.pm' => '$(INST_LIBDIR)/../Filter/CTCP.pm',
							 'Plugin.pm' => '$(INST_LIBDIR)/IRC/Plugin.pm',
						   },
			  'PREREQ_PM' => {
							  'POE'    => 0.06_07,
							  'Carp'   => 0,
							  'Socket' => 0,
							 },
			  'VERSION_FROM' => 'IRC.pm', # finds $VERSION
			  'dist' => { 'COMPRESS' => 'gzip --best' },
			 );

