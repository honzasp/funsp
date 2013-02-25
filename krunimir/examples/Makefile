EXS = $(shell find -name '*.txt')
PDFS = $(shell find -name '*.txt' | sed 's/txt/pdf/')
KRUNIMIR_BIN = ../dist/build/krunimir/krunimir

.PHONY: pdfs
pdfs: $(PDFS)

%.pdf: %.test.svg
	inkscape --export-pdf=$@ $<

%.test.svg: %.txt
	$(KRUNIMIR_BIN) $<
