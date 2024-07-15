module Window(window, background) where

import Graphics.Gloss

width, height, xOffset, yOffset :: Int
width = 600
height = 600
xOffset = 600
yOffset = 150

window :: Display
window = InWindow "Snake" (width, height) (xOffset, yOffset)

background :: Color
background = white


