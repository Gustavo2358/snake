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
import GameMap (loadMap)

render :: StateT Game (Reader Config) Picture
render = do 
  game <- get
  config <- lift ask 
  let apple   = uncurry gridTranslate (appleLoc game) $ color (appleColor game) $ circleSolid (cellSize config / 2)
      snake   = uncurry gridTranslate (snakeHead game) $ color (snakeColor game) $ rectangleSolid (cellSize config) (cellSize config)
      snkTail = pictures (map (\p -> uncurry gridTranslate p $ color (snakeColor game) $ rectangleSolid (cellSize config) (cellSize config)) (snakeTail game))
      texto   = Translate (pointsX config) (pointsY config) $ Scale 0.2 0.2 $ Color white $ Text ("Points: " ++ show (length $ snakeTail game))
      obsPics = pictures (map (\p -> uncurry gridTranslate p $ color (greyN 0.5) $ rectangleSolid (cellSize config) (cellSize config)) (obstacles game))
  return $ pictures [apple, snake, snkTail, drawGrid, texto, obsPics]

renderEnterScore :: StateT Game (Reader Config) Picture
renderEnterScore = do
  game <- get
  let promptText = Translate (-200) 100 $ Scale 0.3 0.3 $ Color white $ Text "Enter Score Limit:"
      inputText = Translate (-100) (-50) $ Scale 0.5 0.5 $ Color white $ Text (scoreInput game)
  return $ pictures [promptText, inputText]

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

renderSuccess :: StateT Game (Reader Config) Picture
renderSuccess = do
  game <- get
  config <- lift ask
  let scoreText = "Score: " ++ show (length (snakeTail game))
  return $ pictures
    [ color (dark green) $ rectangleSolid (fromIntegral (windowWidth config)) (fromIntegral (windowHeight config))
    , Translate (-150) 20 $ Scale 0.4 0.4 $ Color black $ Text "You Win!"
    , Translate (-180) (-90) $ Scale 0.2 0.2 $ Color black $ Text "Press Enter to play again"
    , Translate (-100) (-150) $ Scale 0.3 0.3 $ Color black $ Text scoreText
    ]



updateGameIO :: Float -> Game -> IO Game
updateGameIO delta game = do
  return $ runReader (execStateT (updateGame delta) game) positionsConfig

handleKeysIO :: Event -> Game -> IO Game
handleKeysIO event game = do
  return ( execState (handleKeys event) game)

renderIO :: Game -> IO Picture
renderIO game
  | gameState game == EnterScore = return $ runReader (evalStateT renderEnterScore game) positionsConfig
  | gameWon game = return $ runReader (evalStateT renderSuccess game) positionsConfig
  | gameOver game = return $ runReader (evalStateT renderGameOver game) positionsConfig
  | otherwise = return $ runReader (evalStateT render game) positionsConfig



main :: IO ()
main = do 
  gen <- newStdGen
  obs <- loadMap "map.txt"
  let (applePos, gen') = runReader (createAppleRandomPosition gen obs) positionsConfig
      initialGame = runReader (initialState applePos gen' obs) positionsConfig
  playIO 
    window 
    background 
    120 
    initialGame 
    renderIO 
    handleKeysIO 
    updateGameIO
