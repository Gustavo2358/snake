module Main (main) where

import Control.Monad.Reader
import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game
import KeyHandler
import Window 
import Grid
import Positions
import System.Random (newStdGen)

render :: StateT Game (Reader Config) Picture
render = do 
  game <- get
  config <- lift ask 
  let apple   = uncurry gridTranslate (appleLoc game) $ color (appleColor game) $ circleSolid (cellSize config / 2)
      snake   = uncurry gridTranslate (snakeHead game) $ color (snakeColor game) $ rectangleSolid (cellSize config) (cellSize config)
      snkTail = pictures (map (\p -> uncurry gridTranslate p $ color (snakeColor game) $ rectangleSolid (cellSize config) (cellSize config)) (snakeTail game))
      texto   = Translate (pointsX config) (pointsY config) $ Scale 0.2 0.2 $ Color white $ Text ("Points: " ++ show (length $ snakeTail game))
  return $ pictures [apple, snake, snkTail, drawGrid, texto]

renderGameOver :: StateT Game (Reader Config)  Picture
renderGameOver = do 
  game <- get
  config <- lift ask
  let
    gameOverScreen = color (dark (dark red)) $ rectangleSolid (fromIntegral (windowWidth config)) (fromIntegral (windowHeight config))
    gameOverText   = Translate (-165) 20 $ Scale 0.4 0.4 $ color white $ Text "Game Over!"
    score          = Translate (-70) (-20) $ Scale 0.2 0.2 $ color white $ Text ("Score: " ++ show (length $ snakeTail game))
    playAgainText  = Translate (-180) (-90) $ Scale 0.2 0.2 $ color white $ Text "Press Enter to play again"
  return $ pictures [gameOverScreen, gameOverText, score, playAgainText]

updateGameIO :: Float -> Game -> IO Game
updateGameIO _ game = do
  return $ execState updateGame game

handleKeysIO :: Event -> Game -> IO Game
handleKeysIO event game = do
  return ( execState (handleKeys event) game)

renderIO :: Game -> IO Picture
renderIO game = do
  return $ if gameOver game 
             then runReader (evalStateT renderGameOver game) positionsConfig
             else runReader (evalStateT render game) positionsConfig

main :: IO ()
main = do 
  gen <- newStdGen
  let (applePos, gen') = createAppleRandomPosition gen
  playIO window background 12 (initialState applePos gen') renderIO handleKeysIO updateGameIO
