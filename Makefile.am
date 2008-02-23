bin_PROGRAMS = RPGE
RPGE_SOURCES = main.c video.c video.h main.h mobs.h mobs.c imagestack.h imagestack.c tile.h tile.c guile.h guile.c constants.h event.c event.h window.h window.c sequence.c sequence.h text.c text.h dispatch.c dispatch.h config_file.c config_file.h camera.c camera.h
RPGE_LDADD= lib/libgnu.a
EXTRA_DIST = test.png test_sprite.png test_tile2.png convertors.sh foo.scm key-funcs.scm keys.scm mob_events.scm mob_event_test.scm mobutils.scm stats.scm test.scm .RPGE utils.guile table.guile
SUFFIXES=.c
SUBDIRS=lib
ACLOCAL_AMFLAGS= -I m4
AM_CFLAGS= -I$(top_srcdir)/lib -I$(top_builddir)/lib -Wall
.c.o:
	$(top_srcdir)/convertors.sh $< && $(CC) $(CFLAGS) $(AM_CFLAGS) -c $< && cp $<{.backup,} && rm $<.backup