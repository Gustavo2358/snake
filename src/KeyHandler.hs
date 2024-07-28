module KeyHandler(handleKeys) where


import Graphics.Gloss.Interface.Pure.Game (Event(EventKey), SpecialKey (KeyUp, KeyRight, KeyDown, KeyLeft), Key (SpecialKey), KeyState (Down))
import Snake

handleKeys :: Event -> Snake -> Snake
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) snake = snake {snakeDirection = GoUp}  
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) snake = snake {snakeDirection = GoRight}  
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) snake = snake {snakeDirection = GoDown}  
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) snake = snake {snakeDirection = GoLeft}  
handleKeys _ game = game