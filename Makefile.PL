# $Id: Makefile.PL,v 1.2 2005/01/21 09:08:43 chris Exp $

use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
			  'NAME'	=> 'POE::Component::IRC',
			  'PM'      => { 'IRC.pm' => '$(INST_LIBDIR)/IRC.pm',
							 'IRC-Qnet.pm' => '$(INST_LIBDIR)/IRC/Qnet.pm',
							 'Filter-IRC.pm' => '$(INST_LIBDIR)/../Filter/IRC.pm',
							 'Filter-CTCP.pm' => '$(INST_LIBDIR)/../Filter/CTCP.pm',
						   },
			  'PREREQ_PM' => {
							  'POE'    => 0.06_07,
							  'Carp'   => 0,
							  'Socket' => 0,
							 },
			  'VERSION_FROM' => 'IRC.pm', # finds $VERSION
			  'dist' => { 'COMPRESS' => 'gzip --best' },
			 );

