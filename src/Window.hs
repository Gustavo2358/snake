module Window(window, background) where

import Graphics.Gloss
import Positions (windowWidth, windowHeight, windowXOffset, windowYOffset)

window :: Display
window = InWindow "Snake" (windowWidth, windowHeight) (windowXOffset, windowYOffset)

background :: Color
background = greyN 0.2


