module KeyHandler(handleKeys) where

import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), SpecialKey (KeyUp, KeyRight, KeyDown, KeyLeft), Key (SpecialKey))
import State

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = game {direction = (0, 2)}  
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game = game {direction = (2, 0)}  
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = game {direction = (0, -2)}  
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game = game {direction = (-2, 0)}  
handleKeys _ game = game