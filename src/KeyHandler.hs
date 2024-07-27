module KeyHandler(handleKeys) where


import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), SpecialKey (KeyUp, KeyRight, KeyDown, KeyLeft), Key (SpecialKey))
import Snake

handleKeys :: Event -> Snake -> Snake
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = game {snakeDirection = GoUp}  
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game = game {snakeDirection = GoRight}  
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = game {snakeDirection = GoDown}  
handleKeys (EventKey (SpecialKey KeyLeft) _ _ _) game = game {snakeDirection = GoLeft}  
handleKeys _ game = game