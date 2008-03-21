#Makefile for RPGE documentation
rpgedocsdir = $(datadir)/rpge/doc
rpgedocs_DATA = rpge.info rpge.pdf
EXTRA_DIST = rpge.texinfo fdl.texi rpge.pdf rpge.info
SUFFIXES = .info .pdf
MOSTLYCLEANFILES=rpge.info rpge.pdf rpge.log rpge.aux rpge.cp rpge.fn rpge.vr rpge.tp rpge.ky rpge.pg rpge.toc

.texinfo.info:
	makeinfo $<

.texinfo.pdf:
	texi2pdf $<
