module Main (main) where

import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game
import KeyHandler
import Window 
import Grid
import Positions
import System.Random (newStdGen)

render :: Game -> Picture
render game = pictures [apple, snake, snkTail, drawGrid, texto]
  where
    apple = uncurry gridTranslate (appleLoc game) $ color (appleColor game) $ circleSolid (cellSize / 2)
    snake = uncurry gridTranslate (snakeHead game) $ color (snakeColor game) $ rectangleSolid cellSize cellSize
    snkTail = pictures (map (\p -> uncurry gridTranslate p $ color (snakeColor game) $ rectangleSolid cellSize cellSize) (snakeTail game))
    texto = Translate pointsX pointsY $ Scale 0.2 0.2 $ Color white $ Text ("Points: " ++ show (length $ snakeTail game))

renderGameOver :: Game -> Picture
renderGameOver (Game { snakeTail = st }) = pictures [gameOverScreen, gameOverText, score, playAgainText]
  where
    gameOverScreen = color (dark (dark red)) $ rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)
    gameOverText   = Translate (-165) 20 $ Scale 0.4 0.4 $ color white $ Text "Game Over!"
    score          = Translate (-70) (-20) $ Scale 0.2 0.2 $ color white $ Text ("Score: " ++ show (length st))
    playAgainText  = Translate (-180) (-90) $ Scale 0.2 0.2 $ color white $ Text "Press Enter to play again"

updateGameIO :: Float -> Game -> IO Game
updateGameIO _ game = do
  return $ execState updateGame game

handleKeysIO :: Event -> Game -> IO Game
handleKeysIO event game = do
  return (handleKeys event game)

renderIO :: Game -> IO Picture
renderIO game@(Game {gameOver = gOver}) = do
  return $ if gOver then renderGameOver game else render game

main :: IO ()
main = do 
  gen <- newStdGen
  let (applePos, gen') = createAppleRandomPosition gen
  playIO window background 12 (initialState applePos gen') renderIO handleKeysIO updateGameIO
