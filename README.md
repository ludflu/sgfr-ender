# sgfr-ender - render board diagrams from your SGF game record

![image](https://github.com/user-attachments/assets/590d0f00-9961-41fd-b476-bf4054b5157a)

## Game Rendering

sgf-render creates pdf documents containing a series of go board diagrams, with several moves in each diagram,
suitable for game review. It supports 19x19, 13x13 and 9x9. You can configure how many moves are displayed in each new diagram,
and how many diagrams are displayed per page. 

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

## Combining PDF documents

Each diagram page will be output as a separate PDF document - but you can use one of several 
common tools to combine them into a single document.

For example, you can use pdftk like this:

```
brew install pdftk-java
pdftk test/outtest* cat output all.pdf
```

## Building

Note - this tool relies on the venerable [Data.SGF](https://hackage.haskell.org/package/sgf) package for parsing SGF files.
Unfortunately, this package is a bit old and needs to be updated, so I [forked it](https://github.com/ludflu/sgf) and applied a fix. 
If you're trying to build this package, you'll first need to manually install Data.SGF from my fork, at least until a PR is merged.

```
git clone git@github.com:ludflu/sgf.git
cd sgf
cabal install --lib
cd ..
git clone git@github.com:ludflu/sgfr-ender.git
cd sgfr-ender
cabal build
cabal install
```


