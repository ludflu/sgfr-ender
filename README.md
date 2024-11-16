# sgfr-ender - render board diagrams from your SGF game record

![image](https://github.com/user-attachments/assets/590d0f00-9961-41fd-b476-bf4054b5157a)

## Game Rendering

sgf-render creates pdf documents containing a series of go board diagrams, with several moves in each diagram,
suitable for game review. It supports 19x19, 13x13 and 9x9. You can configure how many moves are displayed in each new diagram,
and how many diagrams are displayed per page. Yes, I know the repo name is spelled wrong.

## Running
```
sgf-render - render sgf files to pdf

Usage: sgf-render [--movesPerDiagram movesPerDiagram]
                  [--diagramsPerPage diagramsPerPage] (-i|--input INPUT)
                  (-o|--output OUTPUT) (-h|--host host) [--port port]
                  [-s|--score-estimate]

  render the sgf file to pdf

Available options:
  --movesPerDiagram movesPerDiagram
                           how many moves to show in each diagram (default: 5)
  --diagramsPerPage diagramsPerPage
                           how many diagrams to show on each page (default: 2)
  -i,--input INPUT         input file path
  -o,--output OUTPUT       output file pattern
  -h,--host host           katago host
  --port port              katago port (default: 8888)
  -s,--score-estimate      Whether to estimate the score of the game using
                           KataGo
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

If you prefer a GUI tool, you can use the [native preview app to combine documents also](https://support.apple.com/guide/preview/combine-pdfs-prvw43696/mac).

## Download

You can find the latest binary on the [releases page](https://github.com/ludflu/sgfr-ender/releases). The initial release only supports Mac OSX / ARM64.

## Building

```
cd sgfr-ender
cabal build
cabal install
```


