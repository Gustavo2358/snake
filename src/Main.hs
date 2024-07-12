module Main (main) where

import Graphics.Gloss

data SnakeGame = Snake
  { snakeLoc :: (Float, Float)
  , appleLoc :: (Float, Float)
  }
  deriving (Show)

initialState :: SnakeGame
initialState =
  Snake
    { snakeLoc = (0, 0)
    , appleLoc = (-2, 0)
    }

width, height, xOffset, yOffset :: Int
width = 600
height = 600
xOffset = 600
yOffset = 150

window :: Display
window = InWindow "Snake" (width, height) (xOffset, yOffset)

background :: Color
background = black

-- apple :: Picture
-- apple = uncurry gridTranslate (appleLoc initialState) $ color appleColor $ circleSolid 15
--   where
--     appleColor = dark red

-- snake :: Picture
-- snake = uncurry gridTranslate (snakeLoc initialState) $ color snakeColor $ rectangleSolid 30 30
--   where
--     snakeColor = green

-- drawing :: Picture
-- drawing = pictures [apple, snake]

gridTranslate :: Float -> Float -> Picture -> Picture
gridTranslate x y = translate (x * 30) (y * 30)

render :: SnakeGame -> Picture
render game = pictures [apple, snake]
  where
    apple = uncurry gridTranslate (appleLoc game) $ color appleColor $ circleSolid 15
    appleColor = dark red
    snake = uncurry gridTranslate (snakeLoc game) $ color snakeColor $ rectangleSolid 30 30
    snakeColor = green

main :: IO ()
main = display window background $ render initialState
