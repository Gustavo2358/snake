module Main (main) where

import Graphics.Gloss
import Snake
import KeyHandler
import Window 
import Grid
import System.Random (randomRIO)

render :: Snake -> Picture
render game = pictures [apple, snake, drawGrid 20 20 600 600]
  where
    apple = uncurry gridTranslate (appleLoc game) $ color appleColor $ circleSolid 15
    appleColor = dark red
    snake = uncurry gridTranslate (snakeLoc game) $ color snakeColor $ rectangleSolid 30 30
    snakeColor = green

main :: IO ()
main = do 
  x <- randomRIO(-9,9)
  y <- randomRIO(-9,9)
  let appPos = (x,y)
  play window background 3 (initialState appPos)  render handleKeys moveSnake
