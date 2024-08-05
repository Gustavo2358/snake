module KeyHandler(handleKeys) where


import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), SpecialKey (KeyUp, KeyRight, KeyDown, KeyLeft, KeyEnter), Key (SpecialKey), KeyState (Down))
import Game

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game    = game {snakeDirection = GoUp}  
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game {snakeDirection = GoRight}  
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game  = game {snakeDirection = GoDown}  
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game  = game {snakeDirection = GoLeft}  
handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) game@(Game {randomGen = rg, gameOver = gOver}) =
  if gOver then initialState pos gen else game
  where
    (pos, gen) = createAppleRandomPosition rg
handleKeys _ game = game