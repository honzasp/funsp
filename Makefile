TEXS = $(shell find . -name '*.tex' -o -name '*.lhs')
IMGS = $(shell find img)
STUFF = paper.aux paper.idx paper.ilg paper.ind paper.log \
				paper.out paper.pdf paper.toc

paper.pdf: $(TEXS) $(IMGS)
	$(MAKE) -C krunimir/examples pdfs
	pdflatex paper
	makeindex paper
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
WATCHED = paper.tex tex krunimir/Krunimir
watch:
	make paper.pdf
	while inotifywait --recursive --event modify --exclude 'swp' $(WATCHED) ; do\
		make paper.pdf; \
	done;
