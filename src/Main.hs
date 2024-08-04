module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Snake
import KeyHandler
import Window 
import Grid
import System.Random (Random (randomR), newStdGen)

render :: Game -> Picture
render game = pictures [apple, snake, snkTail, drawGrid 20 20 600 600, texto]
  where
    apple = uncurry gridTranslate (appleLoc game) $ color (appleColor game) $ circleSolid 15
    snake = uncurry gridTranslate (snakeLoc game) $ color (snakeColor game) $ rectangleSolid 30 30
    snkTail = pictures (map (\p -> uncurry gridTranslate p $ color (snakeColor game) $ rectangleSolid 30 30) (snakeTail game))
    texto = Translate (-295) 275 $ Scale 0.2 0.2 $ Color white $ Text ("Points: " ++ show (length $ snakeTail game))

renderGameOver :: Game -> Picture
renderGameOver (Game { snakeTail = st }) = pictures [gameOverScreen, gameOverText, score]
  where
    gameOverScreen = color (dark (dark red)) $ rectangleSolid 600 600
    gameOverText   = Translate (-165) 20 $ Scale 0.4 0.4 $ color white $ Text "Game Over!"
    score          = Translate (-70) (-20) $ Scale 0.2 0.2 $ color white $ Text ("Score: " ++ show (length st))

updateGame :: Float -> Game -> IO Game
updateGame f game = do
  let newGame = moveSnake f game
  return newGame

handleKeysIO :: Event -> Game -> IO Game
handleKeysIO e w = do
  return (handleKeys e w)

renderIO :: Game -> IO Picture
renderIO game@(Game {gameOver = gOver}) = do
  return $ if gOver then renderGameOver game else render game

main :: IO ()
main = do 
  gen <- newStdGen
  let (x, gen') = randomR (-9,9) gen
  let (y, gen'') = randomR (-9,9) gen'
  let applePos = (x,y)
  playIO window background 12 (initialState applePos gen'') renderIO handleKeysIO updateGame
