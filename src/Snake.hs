module Snake
  ( initialState
  , Snake(..)
  , Direction(..)
  , directionVector
  , moveSnake
  ) where 

import Graphics.Gloss (Color, green, red, dark)

data Snake = Snake
  { snakeLoc :: (Float, Float)
  , appleLoc :: (Float, Float)
  , snakeColor :: Color
  , appleColor :: Color
  , snakeDirection :: Direction
  , snakeTail :: [(Float, Float)]
  }
  deriving (Show)

data Direction = GoUp | GoDown | GoLeft | GoRight deriving (Eq, Show)

directionVector :: Direction -> (Float, Float)
directionVector GoUp    = (0, 1)
directionVector GoDown  = (0, -1)
directionVector GoLeft  = (-1, 0)
directionVector GoRight = (1, 0)

initialState :: (Int, Int) -> Snake
initialState (x, y) = 
  Snake
    { snakeLoc  =  (0.5, 0.5)
    , appleLoc  = (-0.5 + fromIntegral x, -0.5 + fromIntegral y)
    , snakeColor = green
    , appleColor = dark red
    , snakeDirection = GoUp
    , snakeTail = [(0.5, -0.5), (0.5, -1.5), (0.5, -2.5)]
    }

moveSnake :: Float -> Snake -> Snake
moveSnake _ snake = 
  snake 
  {snakeLoc = (x', y') 
  ,snakeTail = calculateNewTailPosition (snakeLoc snake) (snakeTail snake)}
    where
      (x', y') = calculateSnakeMovement (snakeLoc snake) (snakeDirection snake)
  -- where
  --   (x, y) = snakeLoc snake
  --   x' = if x > 9 && snakeDirection snake == GoRight || x < (-9) && snakeDirection snake == GoLeft 
  --     then (-x) 
  --     else x + fst (directionVector (snakeDirection snake))
  --   y' = if y > 9 && snakeDirection snake == GoUp || y < (-9) && snakeDirection snake == GoDown 
  --     then (-y)
  --     else y + snd (directionVector (snakeDirection snake)) 

calculateSnakeMovement :: (Float, Float) -> Direction -> (Float, Float)
calculateSnakeMovement (x, y) dir = 
  ( if x > 9 && dir == GoRight || x < (-9) && dir == GoLeft 
      then (-x) 
      else x + fst (directionVector dir)
  ,if y > 9 && dir == GoUp || y < (-9) && dir == GoDown 
      then (-y)
      else y + snd (directionVector dir))
    
calculateNewTailPosition :: (Float, Float) -> [(Float, Float)] -> [(Float, Float)]
calculateNewTailPosition oldHead oldTail = oldHead : init oldTail