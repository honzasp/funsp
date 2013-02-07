TEXS = $(shell find . -name '*.tex' -o -name '*.lhs')
STUFF = paper.aux paper.idx paper.ilg paper.ind paper.log \
				paper.out paper.pdf paper.toc

paper.pdf: $(TEXS)
	pdflatex paper
	makeindex paper
	pdflatex paper

.PHONY: show
show: paper.pdf
	evince paper.pdf 2>/dev/null &

.PHONY: clean
clean:
	rm $(STUFF)

.PHONY: force
force: clean paper.pdf

.PHONY: watch
WATCHED = tex krunimir/Krunimir
watch:
	while inotifywait --recursive --event modify --exclude 'swp' $(WATCHED) ; do\
		make paper.pdf; \
	done;
