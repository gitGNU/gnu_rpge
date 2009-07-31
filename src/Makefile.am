#Makefile.am for RPGE C source code.
bin_PROGRAMS = rpge
rpge_SOURCES = main.c video.c video.h main.h mobs.h mobs.c imagestack.h imagestack.c tile.h tile.c guile.h guile.c constants.h event.c event.h window.h window.c sequence.c sequence.h text.c text.h dispatch.c dispatch.h config_file.c config_file.h camera.c camera.h path.h path.c guile_mob.c guile_mob.h guile_camera.c guile_camera.h guile_window.c guile_window.h guile_tile.c guile_tile.h guile_text.h guile_text.c guile_global_event.c guile_global_event.h guile_load.c guile_load.h guile_config_file.c guile_config_file.h
rpge_LDADD= $(top_builddir)/lib/libgnu.a
AM_CFLAGS= -I$(top_srcdir)/lib -I$(top_builddir)/lib -Wall -pg
