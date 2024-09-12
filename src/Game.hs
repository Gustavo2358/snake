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

import Control.Monad.State
import Graphics.Gloss (Color, green, red)
import System.Random (StdGen, Random (randomR))
import Positions 

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
    { snakeHead  =  (snakeHeadInitialX positionsConfig, snakeHeadInitialY positionsConfig)
    , appleLoc  = (appleInitialX positionsConfig + fromIntegral randX, appleInitialY positionsConfig + fromIntegral randY)
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

updateGame :: State Game ()
updateGame = do
  game <- get
  let sh = snakeHead game
      al = appleLoc game
      sd = snakeDirection game
      st = snakeTail game
      newSnakeHead = calculateSnakeMovement sh sd
      newSnakeTail = calculateNewTailPosition sh st
      (newAppleX, gen') = randomR (xAppleMinLimit positionsConfig, xAppleMaxLimit positionsConfig) (randomGen game)
      (newAppleY, gen'') = randomR (yAppleMinLimit positionsConfig, yAppleMaxLimit positionsConfig) gen'
      newAppleLoc = (fromIntegral newAppleX + appleInitialX positionsConfig, fromIntegral newAppleY  + appleInitialY positionsConfig)
  if sh == al
    then do
      put game { snakeHead = newSnakeHead
               , snakeTail = if null st then [sh] else head st : newSnakeTail
               , appleLoc = newAppleLoc
               , randomGen = gen''
               }
    else if isCollision newSnakeTail newSnakeHead
      then put game { snakeDirection = Stop, gameOver = True }
      else put game { snakeHead = newSnakeHead, snakeTail = newSnakeTail }

calculateSnakeMovement :: (Float, Float) -> Direction -> (Float, Float)
calculateSnakeMovement (x, y) dir =
  (calcSnakeXPosition, calcSnakeYPosition)
  where
    calcSnakeXPosition 
      | x >= xMaxLimit positionsConfig && dir == GoRight = xMinLimit positionsConfig
      | x <= xMinLimit positionsConfig && dir == GoLeft = xMaxLimit positionsConfig
      | otherwise = x + fst (directionVector dir)
    calcSnakeYPosition
      | y >= yMaxLimit positionsConfig && dir == GoUp = yMinLimit positionsConfig
      | y <= yMinLimit positionsConfig && dir == GoDown = yMaxLimit positionsConfig
      | otherwise = y + snd (directionVector dir)

calculateNewTailPosition :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
calculateNewTailPosition _ [] = []
calculateNewTailPosition oldHead oldTail = oldHead : init oldTail

createAppleRandomPosition :: StdGen -> ((Int, Int), StdGen)
createAppleRandomPosition gen = ((x, y), gen'')
  where 
    (x, gen') = randomR (xAppleMinLimit positionsConfig,xAppleMaxLimit positionsConfig) gen
    (y, gen'') = randomR (yAppleMaxLimit positionsConfig,yAppleMaxLimit positionsConfig) gen'
