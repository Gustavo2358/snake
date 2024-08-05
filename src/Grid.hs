module Grid(drawGrid) where

import Graphics.Gloss 
  (Picture
  ,line
  ,pictures
  ,color
  ,greyN
  )

import Positions

gridColor :: Picture -> Picture
gridColor = color (greyN 0.40)

rows, cols, width, height :: Int
rows = 20
cols = 20
width = 600
height = 600

drawGrid :: Picture
drawGrid =
  pictures $ map gridPosition (horizontalLines ++ verticalLines)
  where
    cellWidth = fromIntegral width / fromIntegral cols
    cellHeight = fromIntegral height / fromIntegral rows
    horizontalLines = [ gridColor $ line [(0, y), (fromIntegral width, y)] | y <- [0, cellHeight .. fromIntegral height] ]
    verticalLines = [ gridColor $ line [(x, 0), (x, fromIntegral height)] | x <- [0, cellWidth .. fromIntegral width] ]
