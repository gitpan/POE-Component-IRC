# $Id: Makefile.PL,v 1.1.1.1 2004/12/22 19:10:57 chris Exp $

use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.

WriteMakefile(
			  'NAME'	=> 'POE::Component::IRC',
			  'PM'      => { 'IRC.pm' => '$(INST_LIBDIR)/IRC.pm',
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

