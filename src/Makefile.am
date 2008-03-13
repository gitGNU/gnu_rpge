#Makefile.am for RPGE C source code.
bin_PROGRAMS = RPGE
RPGE_SOURCES = main.c video.c video.h main.h mobs.h mobs.cgen imagestack.h imagestack.cgen tile.h tile.c guile.h guile.cgen constants.h event.cgen event.h window.h window.cgen sequence.cgen sequence.h text.cgen text.h dispatch.cgen dispatch.h config_file.c config_file.h camera.c camera.h path.h path.c
RPGE_LDADD= $(top_builddir)/lib/libgnu.a
AM_CFLAGS= -I$(top_srcdir)/lib -I$(top_builddir)/lib -Wall
MOSTLYCLEANFILES=dispatch.c imagestack.c mobs.c guile.c event.c window.c sequence.c text.c dispatch.c
.cgen.c:
	$(top_srcdir)/convertors.sh $< $@
	