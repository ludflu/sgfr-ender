{-# LANGUAGE FlexibleContexts #-}

module ScoreLeaderLineChart where

import Diagrams.Backend.Rasterific (B, renderPdf)

import Diagrams.Prelude
import Graphics.SVGFonts
import Graphics.SVGFonts.ReadFont (PreparedFont)


import Diagrams.Core.Points 

type Dia = Diagram B
type Pt = (Double, Double)
type Points = [Pt]

chart :: PreparedFont Double -> [Points] -> [(Dia, Dia -> Dia)] -> [Double] -> [Double] -> Dia
chart font series styles xs ys = mconcat
  [ plotMany styles series dataToFrac
  , horizticks font (map (\x -> ((x-minx)/xrange, showFloor x)) xs)
  , vertticks  font (map (\y -> ((y-miny)/yrange, showFloor y)) ys)
  , box
  ]
  where maxx = last xs
        minx = head xs
        maxy = last ys
        miny = head ys
        xrange = maxx-minx
        yrange = maxy-miny
        dataToFrac (x,y) = ((x-minx)/xrange, (y-miny)/yrange)
        showFloor = show . (floor :: Double -> Integer)

h,w :: Double
h = 7
w = 7

plot :: ((Double, Double) -> (Double, Double)) -> Dia -> (Dia -> Dia) -> Points -> Dia
plot dataToFrac shape lineStyle ps =
    let scalify (x,y) = (x*w,y*h)
        ps' = map (p2 . scalify . dataToFrac) ps
    in (strokeP $ fromVertices ps') # lineStyle
         `beneath` mconcat [ shape # moveTo p | p <- ps' ]

plotMany :: [(Dia, Dia -> Dia)] -> [Points] -> (Pt -> Pt) -> Dia
plotMany styles seriesList dataToFrac =
    mconcat $ zipWith (uncurry (plot dataToFrac)) (styles ++ plotStyles) seriesList

text' :: PreparedFont Double -> String -> Dia
text' font s
  = (set_envelope . fit_height 0.4 . svgText def { textFont = font } $ s)
  # fc black # lw none


legend :: PreparedFont Double -> [(Dia, Dia -> Dia)] -> [String] -> Dia
legend font styles labels = centerXY $
    vcat' with {_sep=0.15} $
      map (\(l,s) -> littleLine s ||| strutX 0.4 ||| text' font l # alignL)
        (zip labels (styles ++ plotStyles))
  where littleLine (d,l) = (strokeP $ fromVertices [ 0^&0, 1^&0 ]) # l
                           <> d # moveTo (0.5^&0)

box :: Dia
box = strokeLoop . closeLine . fromVertices $ [ 0^&0, 0^&h, w^&h, w^&0 ]

vertticks :: PreparedFont Double -> [(Double, String)] -> Dia
vertticks font pairs =
    let textBits = mconcat [ text' font t # alignR # moveTo ((-0.2)^&(y*h)) | (y,t) <- pairs ]
        tickBits =    mconcat [ fromVertices [ 0^&(y*h), 0.1    ^&(y*h) ] | (y,_) <- pairs ]
                   <> mconcat [ fromVertices [ w^&(y*h), (w-0.1)^&(y*h) ] | (y,_) <- pairs ]
                   <> mconcat [ fromVertices [ 0^&(y*h), w^&(y*h)       ] # lc gray # dashingG [ 0.1, 0.1 ] 0 | (y,_) <- pairs ]
    in textBits <> tickBits

horizticks :: PreparedFont Double -> [(Double, String)] -> Dia
horizticks font pairs =
    let textBits = mconcat [ text' font t # moveTo ((x*w)^&(-0.3)) | (x,t) <- pairs ]
        tickBits =    mconcat [ fromVertices [ (x*w)^&0, (x*w)^&0.1     ] | (x,_) <- pairs ]
                   <> mconcat [ fromVertices [ (x*w)^&h, (x*w)^&(h-0.1) ] | (x,_) <- pairs ]
                   <> mconcat [ fromVertices [ (x*w)^&0, (x*w)^&h       ] # lc gray # dashingG [ 0.1, 0.1 ] 0 | (x,_) <- pairs ]
    in textBits <> tickBits

newtype Fill = Fill Bool
type Shape = Dia
type DotStyle = (Shape, Fill)
type LineStyle = Dia -> Dia

plotStyles :: [ (Shape, LineStyle) ]
plotStyles = zipWith3 combineStyles dotStyles colourStyles lineStyles

combineStyles :: DotStyle -> Colour Double -> LineStyle -> (Shape, LineStyle)
combineStyles (d,Fill f) c l =
  ( d # (if f then fcA (c `withOpacity` 0.5) else id) # lc c, lc c . l )

dotStyles :: [DotStyle]
dotStyles = cycle $
    let shapes = map (strokeP)
           [ circle 0.07
           , square 0.1
           , eqTriangle 0.1
           , pentagon 0.1
           , cross 0.07
           , plus 0.07
           , star (StarSkip 2) (pentagon 0.1)
           ]
    in [ (s, Fill b) | b <- [True,False], s <- shapes ]

cross :: Double -> Path V2 Double
cross x = fromVertices [ x^&(-x) , ((-x)^&x) ]
          <> fromVertices [ x^&x , ((-x)^&(-x)) ]

plus :: Double -> Path V2 Double
plus x = cross x # rotate (45 @@ deg)

colourStyles :: [Colour Double]
colourStyles = cycle $ [ red, green, blue, brown ]

lineStyles :: [Dia -> Dia]
lineStyles = cycle $
               [ id, dashingG [0.1,0.1] 0, dashingG [0.02,0.02] 0
               , dashingG [0.1,0.1,0.03,0.1] 0, dashingG [0.1,0.1,0.02,0.02,0.02,0.1] 0 ]