module KeyHandler(handleKeys) where

import Control.Monad  
import Control.Monad.State
import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), SpecialKey (KeyUp, KeyRight, KeyDown, KeyLeft, KeyEnter), Key (SpecialKey), KeyState (Down))
import Game
import Positions (positionsConfig)
import Control.Monad.Reader (runReader)

handleKeys :: Event -> State Game ()
handleKeys (EventKey (SpecialKey KeyUp) Down _ _)    = do modify $ \game -> game {snakeDirection = GoUp}
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) = do modify $ \game -> game {snakeDirection = GoRight}
handleKeys (EventKey (SpecialKey KeyDown) Down _ _)  = do modify $ \game -> game {snakeDirection = GoDown}
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _)  = do modify $ \game -> game {snakeDirection = GoLeft}
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) = do
  game <- get
  when (gameOver game) $ do
    let (pos, gen) = runReader (createAppleRandomPosition $ randomGen game) positionsConfig
    put $ runReader (initialState pos gen) positionsConfig
handleKeys _ = return ()