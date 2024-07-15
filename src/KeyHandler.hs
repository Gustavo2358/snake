module KeyHandler(handleKeys) where


import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), SpecialKey (KeyUp, KeyRight, KeyDown, KeyLeft), Key (SpecialKey))
import State

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = game {direction = GoUp}  
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game = game {direction = GoRight}  
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = game {direction = GoDown}  
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game = game {direction = GoLeft}  
handleKeys _ game = game