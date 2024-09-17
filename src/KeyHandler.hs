module KeyHandler (handleKeys) where

import Control.Monad
import Control.Monad.State
import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), SpecialKey (KeyUp, KeyRight, KeyDown, KeyLeft, KeyEnter), Key (SpecialKey, Char), KeyState (Down))
import Game
import Positions (positionsConfig)
import Control.Monad.Reader (runReader)
import Text.Read (readMaybe)

handleKeys :: Event -> State Game ()
handleKeys (EventKey (SpecialKey key) Down _ _) = do
  game <- get
  case gameState game of
    EnterScore -> handleScoreInput key
    _          -> handleGameInput key
handleKeys (EventKey (Char c) Down _ _) = do
  game <- get
  when (gameState game == EnterScore && c >= '0' && c <= '9') $ do
    put game { scoreInput = scoreInput game ++ [c] }
handleKeys _ = return ()

handleScoreInput :: SpecialKey -> State Game ()
handleScoreInput key = do
  game <- get
  case key of
    KeyEnter -> do
      case readMaybe (scoreInput game) of
        Just score -> do
          let finalScore = score 
          put game { scoreLimit = finalScore, gameState = Playing, gameWon = False, gameOver = False }
        Nothing -> put game { scoreInput = "", gameState = EnterScore }
    _ -> return ()

handleGameInput :: SpecialKey -> State Game ()
handleGameInput key = do
  case key of
    KeyUp    -> changeDirection GoUp
    KeyRight -> changeDirection GoRight
    KeyDown  -> changeDirection GoDown
    KeyLeft  -> changeDirection GoLeft
    KeyEnter -> restartGameIfOverOrWon
    _        -> return ()

restartGameIfOverOrWon :: State Game ()
restartGameIfOverOrWon = do
  game <- get
  when (gameOver game || gameWon game) $ do
    let (pos, gen) = runReader (createAppleRandomPosition (randomGen game) (obstacles game)) positionsConfig
        newState = runReader (initialState pos gen (obstacles game)) positionsConfig
    put newState { scoreLimit = scoreLimit game }

changeDirection :: Direction -> State Game ()
changeDirection newDir = do
  game <- get
  let currentDir = snakeDirection game
  unless (isOpposite newDir currentDir) $ put game { snakeDirection = newDir }

isOpposite :: Direction -> Direction -> Bool
isOpposite GoUp GoDown   = True
isOpposite GoDown GoUp   = True
isOpposite GoLeft GoRight = True
isOpposite GoRight GoLeft = True
isOpposite _ _           = False
