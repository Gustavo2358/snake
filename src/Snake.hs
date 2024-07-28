module Snake
  ( initialState
  , Snake(..)
  , Direction(..)
  , directionVector
  , moveSnake
  ) where

import Graphics.Gloss (Color, green, red, dark)
import System.Random (StdGen, Random (randomR))

data Snake = Snake
  { snakeLoc :: (Float, Float)
  , appleLoc :: (Float, Float)
  , snakeColor :: Color
  , appleColor :: Color
  , snakeDirection :: Direction
  , snakeTail :: [(Float, Float)]
  , randomGen :: StdGen
  }
  deriving (Show)

data Direction = GoUp | GoDown | GoLeft | GoRight deriving (Eq, Show)

directionVector :: Direction -> (Float, Float)
directionVector GoUp    = (0, 1)
directionVector GoDown  = (0, -1)
directionVector GoLeft  = (-1, 0)
directionVector GoRight = (1, 0)

initialState :: (Int, Int) -> StdGen -> Snake
initialState (x, y) gen =
  Snake
    { snakeLoc  =  (0.5, 0.5)
    , appleLoc  = (-0.5 + fromIntegral x, -0.5 + fromIntegral y)
    , snakeColor = green
    , appleColor = dark red
    , snakeDirection = GoUp
    , snakeTail = []
    , randomGen = gen
    }

moveSnake :: Float -> Snake -> Snake
moveSnake _ snake@(Snake {snakeLoc = sl, appleLoc = al, snakeDirection = sd, snakeTail = st}) 
  | sl == al  = snake {snakeLoc  = calculateSnakeMovement sl sd
                      ,snakeTail = if null st then sl : calculateNewTailPosition sl st else head st : calculateNewTailPosition sl st
                      ,appleLoc  = newAppleLoc
                      ,randomGen = newRandomGen
                      }
  | otherwise = snake {snakeLoc  = calculateSnakeMovement sl sd
                      ,snakeTail = calculateNewTailPosition sl st
                      }
  where
    (newAppleX, gen1)         = randomR (-9 :: Int, 9) (randomGen snake)
    (newAppleY, newRandomGen) = randomR (-9 :: Int, 9) gen1
    newAppleLoc               = (fromIntegral newAppleX + 0.5, fromIntegral newAppleY + 0.5)

calculateSnakeMovement :: (Float, Float) -> Direction -> (Float, Float)
calculateSnakeMovement (x, y) dir =
  ( if x > 9 && dir == GoRight || x < (-9) && dir == GoLeft
      then (-x)
      else x + fst (directionVector dir)
  ,if y > 9 && dir == GoUp || y < (-9) && dir == GoDown
      then (-y)
      else y + snd (directionVector dir))

calculateNewTailPosition :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
calculateNewTailPosition _ [] = []
calculateNewTailPosition oldHead oldTail = oldHead : init oldTail