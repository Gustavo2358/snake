module Main (main) where

import Graphics.Gloss
import State
import KeyHandler
import Window 

gridTranslate :: Float -> Float -> Picture -> Picture
gridTranslate x y = translate (x * 30) (y * 30)

moveSnake :: Float -> GameState -> GameState
moveSnake seconds game = game {snakeLoc = (x', y')}
  where
    (x, y) = snakeLoc game 
    x' = x + fst (directionVector (direction game)) * seconds
    y' = y + snd (directionVector (direction game)) * seconds

render :: GameState -> Picture
render game = pictures [apple, snake, drawGrid 20 20 600 600]
  where
    apple = uncurry gridTranslate (appleLoc game) $ color appleColor $ circleSolid 15
    appleColor = dark red
    snake = uncurry gridTranslate (snakeLoc game) $ color snakeColor $ rectangleSolid 30 30
    snakeColor = green

update :: Float -> GameState -> GameState
update = moveSnake

drawGrid :: Int -> Int -> Int -> Int -> Picture
drawGrid rows cols width height =
  pictures $ map (gridTranslate (-10) (-10)) (horizontalLines ++ verticalLines)
  where
    cellWidth = fromIntegral width / fromIntegral cols
    cellHeight = fromIntegral height / fromIntegral rows

    horizontalLines = [ line [(0, y), (fromIntegral width, y)] | y <- [0, cellHeight .. fromIntegral height] ]
    verticalLines = [ line [(x, 0), (x, fromIntegral height)] | x <- [0, cellWidth .. fromIntegral width] ]

main :: IO ()
main = play window background 2 initialState render handleKeys update
