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
  , GameState(..)
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Graphics.Gloss (Color, green, red)
import System.Random (StdGen, Random (randomR))
import Positions 
import GameMap (ObstaclesMap)

data Game = Game
  { snakeHead :: (Float, Float)
  , appleLoc :: (Float, Float)
  , snakeColor :: Color
  , appleColor :: Color
  , snakeDirection :: Direction
  , snakeTail :: [(Float, Float)]
  , randomGen :: StdGen
  , gameOver :: Bool
  , gameWon :: Bool
  , elapsedTime :: Float
  , idleTime :: Float
  , obstacles :: ObstaclesMap
  , scoreLimit :: Int
  , gameState :: GameState
  , scoreInput :: String 
  }
  deriving (Show)


data GameState = Playing | GameOver | Victory | EnterScore deriving (Eq, Show)

data Direction = GoUp | GoDown | GoLeft | GoRight | Stop deriving (Eq, Show)

directionVector :: Direction -> (Float, Float)
directionVector GoUp    = (0, 1)
directionVector GoDown  = (0, -1)
directionVector GoLeft  = (-1, 0)
directionVector GoRight = (1, 0)
directionVector Stop    = (0, 0)

initialState :: (Float, Float) -> StdGen -> ObstaclesMap -> Reader Config Game
initialState (randX, randY) gen obs = do
  config <- ask
  return Game
    { snakeHead = (snakeHeadInitialX config, snakeHeadInitialY config)
    , appleLoc = (randX, randY)
    , snakeColor = green
    , appleColor = red
    , snakeDirection = Stop
    , snakeTail = []
    , randomGen = gen
    , gameOver = False
    , gameWon = False
    , elapsedTime = 0
    , idleTime = 0.15
    , obstacles = obs
    , scoreLimit = 0
    , gameState = EnterScore
    , scoreInput = ""
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
    then put game {elapsedTime = elapsedTime game + delta}
    else do
      newSnakeHead <- lift $ calculateSnakeMovement (snakeHead game) (snakeDirection game)
      let sh = snakeHead game
          al = appleLoc game
          st = snakeTail game
          newSnakeTail = calculateNewTailPosition sh st
      if length newSnakeTail >= scoreLimit game
        then do
          put game { gameWon = True, elapsedTime = 0 }
      else if newSnakeHead == al
        then do
          (newAppleLoc, gen) <- lift $ createAppleRandomPosition (randomGen game) (obstacles game)
          put game { snakeHead = newSnakeHead
                   , snakeTail = if null st then [sh] else head st : newSnakeTail
                   , appleLoc = newAppleLoc
                   , randomGen = gen
                   , elapsedTime = 0
                   , idleTime = idleTime game * idleTimeDiminishingFactor config
                   }
      else if isCollision newSnakeTail newSnakeHead || isCollision (obstacles game) newSnakeHead
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

createAppleRandomPosition :: StdGen -> ObstaclesMap -> Reader Config ((Float, Float), StdGen)
createAppleRandomPosition gen obs = do 
  config <- ask
  let (x, gen') = randomR (xAppleMinLimit config,xAppleMaxLimit config) gen
      (y, gen'') = randomR (yAppleMinLimit config,yAppleMaxLimit config) gen'
      newAppleLoc = (fromIntegral x + appleInitialX config, fromIntegral y  + appleInitialY config)
  if newAppleLoc `elem` obs 
    then createAppleRandomPosition gen'' obs
    else return (newAppleLoc, gen'')