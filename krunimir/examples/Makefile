EXS = $(shell find -name '*.txt')
PDFS = $(shell find -name '*.txt' | sed 's/txt/pdf/')
SVGS = $(shell find -name '*.txt' | sed 's/txt/test.svg/')
KRUNIMIR_BIN = ../dist/build/krunimir/krunimir

.PHONY: pdfs
.PHONY: svgs
pdfs: $(PDFS)
svgs: $(SVGS)

%.pdf: %.test.svg
	inkscape --export-pdf=$@ $<

%.test.svg: %.txt
	$(KRUNIMIR_BIN) $<
