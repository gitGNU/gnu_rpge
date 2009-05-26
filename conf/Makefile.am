#RPGE conf Makefile.am, handling configuration issues.
confdir=$(DESTDIR)$(datadir)/rpge/conf
conf_DATA=default.conf default_new.conf
noinst_DATA=skeleton.conf skeleton_new.conf
EXTRA_DIST=default.confin skeleton.confin default_new.confin skeleton_new.confin
SUFFIXES = .confin
MOSTLYCLEANFILES=default.conf skeleton.conf default_new.conf skeleton_new.conf

.confin.conf:
	cat $< | sed s'|'datadir'|'$(datadir)/rpge'|'g > $@