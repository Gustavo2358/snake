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

import Control.Monad.Reader
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
  , elapsedTime :: Float
  , idleTime :: Float
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
    , elapsedTime = 0
    , idleTime = 0.1
    }

isCollision :: [(Float, Float)] -> (Float, Float) -> Bool
isCollision [] _ = False
isCollision (x:xs) p  
  | x == p    = True
  | otherwise = isCollision xs p  

updateGame :: Float -> StateT Game (Reader Config) ()
updateGame delta = do
  game <- get
  config <- lift ask
  if elapsedTime game < idleTime game
    then do
      put game {elapsedTime = elapsedTime game + delta}
    else do
      newSnakeHead <- lift $ calculateSnakeMovement (snakeHead game) (snakeDirection game)
      let sh = snakeHead game
          al = appleLoc game
          st = snakeTail game
          newSnakeTail = calculateNewTailPosition sh st
          (newAppleX, gen') = randomR (xAppleMinLimit config, xAppleMaxLimit config) (randomGen game)
          (newAppleY, gen'') = randomR (yAppleMinLimit config, yAppleMaxLimit config) gen'
          newAppleLoc = (fromIntegral newAppleX + appleInitialX config, fromIntegral newAppleY  + appleInitialY config)
      if sh == al
        then do
          put game { snakeHead = newSnakeHead
                  , snakeTail = if null st then [sh] else head st : newSnakeTail
                  , appleLoc = newAppleLoc
                  , randomGen = gen''
                  , elapsedTime = 0
                  , idleTime = idleTime game * idleTimeDiminishingFactor config
                  }
        else if isCollision newSnakeTail newSnakeHead
          then put game { snakeDirection = Stop, gameOver = True, elapsedTime = 0 }
          else put game { snakeHead = newSnakeHead, snakeTail = newSnakeTail, elapsedTime = 0 }

calculateSnakeMovement :: (Float, Float) -> Direction -> Reader Config (Float, Float)
calculateSnakeMovement (x, y) dir = do
  config <- ask
  let calcSnakeXPosition
        | x >= xMaxLimit config && dir == GoRight = xMinLimit config
        | x <= xMinLimit config && dir == GoLeft  = xMaxLimit config
        | otherwise = x + fst (directionVector dir)
      calcSnakeYPosition
        | y >= yMaxLimit config && dir == GoUp    = yMinLimit config
        | y <= yMinLimit config && dir == GoDown  = yMaxLimit config
        | otherwise = y + snd (directionVector dir)
  return (calcSnakeXPosition, calcSnakeYPosition)

calculateNewTailPosition :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
calculateNewTailPosition _ [] = []
calculateNewTailPosition oldHead oldTail = oldHead : init oldTail

createAppleRandomPosition :: StdGen -> ((Int, Int), StdGen)
createAppleRandomPosition gen = ((x, y), gen'')
  where 
    (x, gen') = randomR (xAppleMinLimit positionsConfig,xAppleMaxLimit positionsConfig) gen
    (y, gen'') = randomR (yAppleMaxLimit positionsConfig,yAppleMaxLimit positionsConfig) gen'
