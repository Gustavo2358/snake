module Snake
  ( initialState
  , Game(..)
  , Direction(..)
  , directionVector
  , isCollision
  , updateGame
  ) where

import Graphics.Gloss (Color, green, red)
import System.Random (StdGen, Random (randomR))

data Game = Game
  { snakeHead :: (Float, Float)
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
    { snakeHead  =  (0.5, 0.5)
    , appleLoc  = (-0.5 + fromIntegral x, -0.5 + fromIntegral y)
    , snakeColor = green
    , appleColor = red
    , snakeDirection = Stop
    , snakeTail = []
    , randomGen = gen
    , gameOver  = False
    }

isCollision :: [(Float, Float)] -> (Float, Float) -> Bool
isCollision [] _ = False
isCollision (x:xs) p  
  | x == p    = True
  | otherwise = isCollision xs p  

updateGame :: Float -> Game -> Game
updateGame _ game@(Game {snakeHead = sh, appleLoc = al, snakeDirection = sd, snakeTail = st})
  | sh == al  = game {snakeHead  = newSnakeHead
                      ,snakeTail = if null st then [sh] else head st : newSnakeTail
                      ,appleLoc  = newAppleLoc
                      ,randomGen = newRandomGen
                      }
  | otherwise = if isCollision  newSnakeTail newSnakeHead 
                then
                  game { snakeDirection = Stop
                        , gameOver       = True
                        }
                else  
                  game {snakeHead  = newSnakeHead
                      ,snakeTail = newSnakeTail
                      }
  where
    newSnakeHead              = calculateSnakeMovement sh sd
    newSnakeTail              = calculateNewTailPosition sh st
    (newAppleX, gen1)         = randomR (-9 :: Int, 9) (randomGen game)
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
