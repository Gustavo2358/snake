module Main (main) where

import Graphics.Gloss
import Snake
import KeyHandler
import Window 
import Grid
import System.Random (Random (randomR), newStdGen)

render :: Snake -> Picture
render game = pictures [apple, snake, snkTail, drawGrid 20 20 600 600, texto]
  where
    apple = uncurry gridTranslate (appleLoc game) $ color (appleColor game) $ circleSolid 15
    snake = uncurry gridTranslate (snakeLoc game) $ color (snakeColor game) $ rectangleSolid 30 30
    snkTail = pictures (map (\p -> uncurry gridTranslate p $ color (snakeColor game) $ rectangleSolid 30 30) (snakeTail game))
    texto = Translate (-295) 275 $ Scale 0.2 0.2 $ Color black $ Text ("Points: " ++ show (length $ snakeTail game))


main :: IO ()
main = do 
  gen <- newStdGen
  let (x, gen') = randomR (-9,9) gen
  let (y, gen'') = randomR (-9,9) gen'
  let applePos = (x,y)
  play window background 8 (initialState applePos gen'') render handleKeys moveSnake
