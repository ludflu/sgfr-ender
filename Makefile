all:
	cabal build sgf-render
	cabal install sgf-render --overwrite-policy=always

clean:
	rm test/*.pdf

run:
	sgf-render -i ./65761210-307-mannesmann-ludflu215.sgf -o test/outtest --movesPerDiagram 50
