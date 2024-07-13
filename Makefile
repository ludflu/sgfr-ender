all:
	cabal build sgf-render
	cabal install sgf-render --overwrite-policy=always

test:
	cabal test

clean:
	rm test/*.pdf

run:
	sgf-render \
		-i ./65761210-307-mannesmann-ludflu215.sgf \
		-o test/outtest \
		--movesPerDiagram 25 \
		--diagramsPerPage 4
