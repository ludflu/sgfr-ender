# sgfr-ender - render board diagrams from your SGF game record

## Running
```
sgf-render - render sgf files to pdf

Usage: sgf-render [--movesPerDiagram movesPerDiagram]
                  [--diagramsPerPage diagramsPerPage] (-i|--input INPUT)
                  (-o|--output OUTPUT)

  render the sgf file to pdf

Available options:
  --movesPerDiagram movesPerDiagram
                           how many moves to show in each diagram (default: 5)
  --diagramsPerPage diagramsPerPage
                           how many diagrams to show on each page (default: 2)
  -i,--input INPUT         input file path
  -o,--output OUTPUT       output file pattern
  -h,--help                Show this help text
```

## Building
`
cabal build
cabal install
`


