module Game
  ( initialState
  , Game(..)
  , Direction(..)
  , directionVector
  , isCollision
  , updateGame
  , createAppleRandomPosition
  , calculateSnakeMovement
  , calculateNewTailPosition
  ) where

import Graphics.Gloss (Color, green, red)
import System.Random (StdGen, Random (randomR))
import Positions 
  ( xMaxLimit
  , xMinLimit
  , yMaxLimit
  , yMinLimit
  , xAppleMinLimit
  , xAppleMaxLimit
  , yAppleMaxLimit
  , yAppleMinLimit
  , snakeHeadInitialX
  , snakeHeadInitialY
  , appleInitialX
  , appleInitialY
  )

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
initialState (randX, randY) gen =
  Game
    { snakeHead  =  (snakeHeadInitialX, snakeHeadInitialY)
    , appleLoc  = (appleInitialX + fromIntegral randX, appleInitialY + fromIntegral randY)
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
                      ,randomGen = gen''
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
    (newAppleX, gen')         = randomR (xAppleMinLimit, xAppleMaxLimit) (randomGen game)
    (newAppleY, gen'') = randomR (yAppleMinLimit, yAppleMaxLimit) gen'
    newAppleLoc               = (fromIntegral newAppleX + appleInitialX, fromIntegral newAppleY + appleInitialY)

calculateSnakeMovement :: (Float, Float) -> Direction -> (Float, Float)
calculateSnakeMovement (x, y) dir =
  (calcSnakeXPosition, calcSnakeYPosition)
  where
    calcSnakeXPosition 
      | x >= xMaxLimit && dir == GoRight = xMinLimit
      | x <= xMinLimit && dir == GoLeft = xMaxLimit
      | otherwise = x + fst (directionVector dir)
    calcSnakeYPosition
      | y >= yMaxLimit && dir == GoUp = yMinLimit
      | y <= yMinLimit && dir == GoDown = yMaxLimit
      | otherwise = y + snd (directionVector dir)

calculateNewTailPosition :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
calculateNewTailPosition _ [] = []
calculateNewTailPosition oldHead oldTail = oldHead : init oldTail

createAppleRandomPosition :: StdGen -> ((Int, Int), StdGen)
createAppleRandomPosition gen = ((x, y), gen'')
  where 
    (x, gen') = randomR (xAppleMinLimit,xAppleMaxLimit) gen
    (y, gen'') = randomR (yAppleMaxLimit,yAppleMaxLimit) gen'
