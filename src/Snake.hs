module Snake
  ( initialState
  , Game(..)
  , Direction(..)
  , directionVector
  , moveSnake
  , isCollision
  ) where

import Graphics.Gloss (Color, green, red, dark)
import System.Random (StdGen, Random (randomR))

data Game = Game
  { snakeLoc :: (Float, Float)
  , appleLoc :: (Float, Float)
  , snakeColor :: Color
  , appleColor :: Color
  , snakeDirection :: Direction
  , snakeTail :: [(Float, Float)]
  , randomGen :: StdGen
  , gameOver :: Bool
  }
  deriving (Show)

data Direction = GoUp | GoDown | GoLeft | GoRight | Stop deriving (Eq, Show)

directionVector :: Direction -> (Float, Float)
directionVector GoUp    = (0, 1)
directionVector GoDown  = (0, -1)
directionVector GoLeft  = (-1, 0)
directionVector GoRight = (1, 0)
directionVector Stop    = (0, 0)

initialState :: (Int, Int) -> StdGen -> Game
initialState (x, y) gen =
  Game
    { snakeLoc  =  (0.5, 0.5)
    , appleLoc  = (-0.5 + fromIntegral x, -0.5 + fromIntegral y)
    , snakeColor = green
    , appleColor = dark red
    , snakeDirection = GoUp
    , snakeTail = []
    , randomGen = gen
    , gameOver  = False
    }

isCollision :: [(Float, Float)] -> (Float, Float) -> Bool
isCollision [] _ = False
isCollision (x:xs) p  
  | x == p    = True
  | otherwise = isCollision xs p  

moveSnake :: Float -> Game -> Game
moveSnake _ snake@(Game {snakeLoc = sl, appleLoc = al, snakeDirection = sd, snakeTail = st}) 
  | sl == al  = snake {snakeLoc  = newSnakeHead
                      ,snakeTail = if null st then sl : newSnakeTail else head st : newSnakeTail
                      ,appleLoc  = newAppleLoc
                      ,randomGen = newRandomGen
                      }
  | otherwise = if isCollision  newSnakeTail newSnakeHead 
                then
                  snake { snakeDirection = Stop
                        , gameOver       = True
                        }
                else  
                  snake {snakeLoc  = newSnakeHead
                      ,snakeTail = newSnakeTail
                      }
  where
    newSnakeHead              = calculateSnakeMovement sl sd
    newSnakeTail              = calculateNewTailPosition sl st
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
