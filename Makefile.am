bin_PROGRAMS = RPGE
RPGE_SOURCES = main.c video.c video.h main.h mobs.h mobs.cgen imagestack.h imagestack.cgen tile.h tile.c guile.h guile.cgen constants.h event.cgen event.h window.h window.cgen sequence.cgen sequence.h text.cgen text.h dispatch.cgen dispatch.h config_file.c config_file.h camera.c camera.h
RPGE_LDADD= lib/libgnu.a
EXTRA_DIST = test.png test_sprite.png test_tile2.png convertors.sh foo.scm key-funcs.scm keys.scm mob_events.scm mob_event_test.scm mobutils.scm stats.scm test.scm .RPGE utils.guile table.guile doc/RPGE.info doc/RPGE.texinfo doc/fdl.texi doc/RPGE.pdf cleanup_loop.sh
SUFFIXES=.cgen
SUBDIRS=lib
ACLOCAL_AMFLAGS= -I m4
AM_CFLAGS= -I$(top_srcdir)/lib -I$(top_builddir)/lib -Wall
.cgen.c:
	$(top_srcdir)/convertors.sh $< && echo $@ >> to_be_deleted.log
		
mostlyclean-local:
	$(top_srcdir)/cleanup_loop.sh $(top_srcdir)/to_be_deleted.log
	
clean-local: mostlyclean-local
