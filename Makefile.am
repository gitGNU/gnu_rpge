#RPGE main Makefile.am.
RPGEdatadir = $(datadir)/RPGE
RPGEdata_DATA = default.conf
EXTRA_DIST = convertors.sh  .RPGE  cleanup_loop.sh default.confin
SUFFIXES=.cgen .confin
SUBDIRS=lib scm doc images src
ACLOCAL_AMFLAGS= -I m4	
MOSTLYCLEANFILES=default.conf
.confin.conf:
	cat $< | sed s'|'datadir'|'$(RPGEdatadir)'|'g > $@
