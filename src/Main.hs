module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)

data SnakeGame = Snake
  { snakeLoc :: (Float, Float)
  , appleLoc :: (Float, Float)
  , direction :: (Float, Float)
  }
  deriving (Show)

initialState :: SnakeGame
initialState =
  Snake
    { snakeLoc  =  (0, 0)
    , appleLoc  = (-2, 0)
    , direction = (0, 1)
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

gridTranslate :: Float -> Float -> Picture -> Picture
gridTranslate x y = translate (x * 30) (y * 30)

moveSnake :: SnakeGame -> SnakeGame
moveSnake game = game {snakeLoc = (x', y')}
  where
    (x, y) = snakeLoc game 
    x' = x + fst (direction game) 
    y' = y + snd (direction game)

render :: SnakeGame -> Picture
render game = pictures [apple, snake]
  where
    apple = uncurry gridTranslate (appleLoc game) $ color appleColor $ circleSolid 15
    appleColor = dark red
    snake = uncurry gridTranslate (snakeLoc game) $ color snakeColor $ rectangleSolid 30 30
    snakeColor = green

-- update :: ViewPort -> Float -> SnakeGame -> SnakeGame
update :: ViewPort -> Float -> SnakeGame -> SnakeGame
update _ _ = moveSnake

main :: IO ()
main = simulate window background 1 initialState render update
