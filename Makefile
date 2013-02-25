TEXS = $(shell find . -name '*.tex' -o -name '*.lhs')
IMGS = $(shell find img)
BIBS = $(shell find . -name '*.bib')
STUFF = paper.aux paper.idx paper.ilg paper.ind paper.log \
				paper.out paper.pdf paper.toc

RERUN = "(There were undefined references|Rerun to get (cross-references|the bars) right)"
RERUNBIB = "No file.*\.bbl|Citation.*undefined"

PDFLATEX = pdflatex
PDFVIEWER = evince

paper.pdf: $(TEXS) $(IMGS) $(BIBS) examples
	$(PDFLATEX) paper
	makeindex paper
	egrep -q $(RERUNBIB) paper.log && bibtex paper ; true
	egrep -q $(RERUN) paper.log && $(PDFLATEX) paper ; true
	egrep -q $(RERUN) paper.log && $(PDFLATEX) paper ; true

.PHONY: examples
examples:
	$(MAKE) -C krunimir/examples pdfs

.PHONY: view clean force watch
view:
	$(PDFVIEWER) paper.pdf 2>/dev/null &

clean:
	rm $(STUFF) || true

force: clean paper.pdf

WATCHED = paper.tex $(BIBS) tex krunimir/Krunimir krunimir/examples
watch:
	$(MAKE) -C . paper.pdf
	while inotifywait --recursive --event modify --exclude 'swp' $(WATCHED) || true; do\
		$(MAKE) -C . paper.pdf; \
	done;
