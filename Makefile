all:
	cabal build sgf-render
	cabal install sgf-render --overwrite-policy=always

run:
	sgf-render -i ./65761210-307-mannesmann-ludflu215.sgf -o outtest.pdf
