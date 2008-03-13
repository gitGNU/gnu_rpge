#Makefile for RPGE documentation
RPGEdocsdir = $(datadir)/RPGE/doc
RPGEdocs_DATA = RPGE.info RPGE.pdf
EXTRA_DIST = RPGE.texinfo fdl.texi
SUFFIXES = .info .pdf
MOSTLYCLEANFILES=RPGE.info RPGE.pdf RPGE.log RPGE.aux RPGE.cp RPGE.fn RPGE.vr RPGE.tp RPGE.ky RPGE.pg RPGE.toc
.texinfo.info:
	makeinfo $< 
.texinfo.pdf:
	texi2pdf $<