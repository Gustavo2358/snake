module Grid(drawGrid, gridTranslate) where

import Graphics.Gloss (Picture, line, pictures, translate)

-- tela 600 x 600, cellSize 30 => 20 x 20 celulas
cellSize :: Float
cellSize = 30

-- Faz o snake andar no grid
gridTranslate :: Float -> Float -> Picture -> Picture
gridTranslate x y = translate (x * cellSize) (y * cellSize)

drawGrid :: Int -> Int -> Int -> Int -> Picture
drawGrid rows cols width height =
  pictures $ map (gridTranslate (-10) (-10)) (horizontalLines ++ verticalLines)
  where
    cellWidth = fromIntegral width / fromIntegral cols
    cellHeight = fromIntegral height / fromIntegral rows

    horizontalLines = [ line [(0, y), (fromIntegral width, y)] | y <- [0, cellHeight .. fromIntegral height] ]
    verticalLines = [ line [(x, 0), (x, fromIntegral height)] | x <- [0, cellWidth .. fromIntegral width] ]
