.PHONY: test

all:
	cabal build sgf-render
	cabal install sgf-render --overwrite-policy=always

test:
	cabal test

clean:
	rm -f test/*.pdf
	rm -f all.pdf


run19:
	cabal run sgf-render -- \
		-i ./65761210-307-mannesmann-ludflu215.sgf \
		-o test/outtest \
		--movesPerDiagram 4 \
		--diagramsPerPage 4 \
		-s

run9:
	sgf-render \
		-i ./41312474-050-ludflu215-jjprnty.sgf \
		-o test/outtest \
		--movesPerDiagram 50 \
		--diagramsPerPage 4 \

run13:
	sgf-render \
		-i ./58773515-120-ludflu215-jjprnty.sgf \
		-o test/outtest \
		--movesPerDiagram 50 \
		--diagramsPerPage 4



combine:
	rm -f all.pdf
	pdftk test/outtest* cat output all.pdf

debug:
	cabal repl sgf-render
