module KeyHandler(handleKeys) where

import Control.Monad
import Control.Monad.State
import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), SpecialKey (KeyUp, KeyRight, KeyDown, KeyLeft, KeyEnter), Key (SpecialKey), KeyState (Down))
import Game
import Positions (positionsConfig)
import Control.Monad.Reader (runReader)

handleKeys :: Event -> State Game ()
handleKeys (EventKey (SpecialKey key) Down _ _) =
  case key of
    KeyUp    -> changeDirection GoUp
    KeyRight -> changeDirection GoRight
    KeyDown  -> changeDirection GoDown
    KeyLeft  -> changeDirection GoLeft
    KeyEnter -> restartGameIfOver
    _        -> return ()
handleKeys _ = return ()

changeDirection :: Direction -> State Game ()
changeDirection newDir = do
  game <- get
  let currentDir = snakeDirection game
  unless (isOpposite newDir currentDir) $ do put game { snakeDirection = newDir }


isOpposite :: Direction -> Direction -> Bool
isOpposite GoUp GoDown   = True
isOpposite GoDown GoUp   = True
isOpposite GoLeft GoRight = True
isOpposite GoRight GoLeft = True
isOpposite _ _           = False

restartGameIfOver :: State Game ()
restartGameIfOver = do
  game <- get
  when (gameOver game) $ do
    let (pos, gen) = runReader (createAppleRandomPosition (randomGen game) (obstacles game)) positionsConfig
        newState = runReader (initialState pos gen (obstacles game)) positionsConfig
    put newState
