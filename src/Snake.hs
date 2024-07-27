module Snake
  ( initialState
  , Snake(..)
  , Direction(..)
  , directionVector
  , moveSnake
  ) where 

data Snake = Snake
  { snakeLoc :: (Float, Float)
  , appleLoc :: (Float, Float)
  , snakeDirection :: Direction
  }
  deriving (Show)

data Direction = GoUp | GoDown | GoLeft | GoRight deriving (Eq, Show)

directionVector :: Direction -> (Float, Float)
directionVector GoUp    = (0, 1)
directionVector GoDown  = (0, -1)
directionVector GoLeft  = (-1, 0)
directionVector GoRight = (1, 0)

initialState :: Snake
initialState =
  Snake
    { snakeLoc  =  (0.5, 0.5)
    , appleLoc  = (-0.5, -0.5)
    , snakeDirection = GoUp
    }

moveSnake :: Float -> Snake -> Snake
moveSnake _ snake = snake {snakeLoc = (x', y')}
  where
    (x, y) = snakeLoc snake
    x' = if x > 9 && snakeDirection snake == GoRight || x < (-9) && snakeDirection snake == GoLeft 
      then (-x) 
      else x + fst (directionVector (snakeDirection snake))
    y' = if y > 9 && snakeDirection snake == GoUp || y < (-9) && snakeDirection snake == GoDown 
      then (-y)
      else y + snd (directionVector (snakeDirection snake)) 
