#RPGE conf Makefile.am, handling configuration issues.
confdir=$(datadir)/rpge/conf
conf_DATA=default.conf
noinst_DATA=skeleton.conf
EXTRA_DIST=default.confin skeleton.confin 
SUFFIXES = .confin
MOSTLYCLEANFILES=default.conf skeleton.conf

.confin.conf:
	cat $< | sed s'|'datadir'|'$(datadir)/rpge'|'g > $@