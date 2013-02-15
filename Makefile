TEXS = $(shell find . -name '*.tex' -o -name '*.lhs')
IMGS = $(shell find img)
BIBS = $(shell find . -name '*.bib')
STUFF = paper.aux paper.idx paper.ilg paper.ind paper.log \
				paper.out paper.pdf paper.toc

paper.pdf: $(TEXS) $(IMGS) $(BIBS)
	$(MAKE) -C krunimir/examples pdfs
	pdflatex paper
	makeindex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

.PHONY: show
show: paper.pdf
	evince paper.pdf 2>/dev/null &

.PHONY: clean
clean:
	rm $(STUFF) || true

.PHONY: force
force: clean paper.pdf

.PHONY: watch
WATCHED = paper.tex $(BIBS) tex krunimir/Krunimir
watch:
	make paper.pdf
	while inotifywait --recursive --event modify --exclude 'swp' $(WATCHED) || true; do\
		make paper.pdf; \
	done;
