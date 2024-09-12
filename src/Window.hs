module Window(window, background) where

import Graphics.Gloss
import Positions 

window :: Display
window = InWindow "Snake" (windowWidth positionsConfig, windowHeight positionsConfig) (windowXOffset positionsConfig, windowYOffset positionsConfig)

background :: Color
background = greyN 0.2


