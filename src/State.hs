module State
  ( initialState
  , GameState(..)
  , Direction(..)
  , directionVector
  ) where 

data GameState = Snake
  { snakeLoc :: (Float, Float)
  , appleLoc :: (Float, Float)
  , direction :: Direction
  }
  deriving (Show)

step :: Float
step = 2

data Direction = GoUp | GoDown | GoLeft | GoRight deriving (Eq, Show)

directionVector :: Direction -> (Float, Float)
directionVector GoUp    = (0, step)
directionVector GoDown  = (0, -step)
directionVector GoLeft  = (-step, 0)
directionVector GoRight = (step, 0)

initialState :: GameState
initialState =
  Snake
    { snakeLoc  =  (0.5, 0.5)
    , appleLoc  = (-0.5, -0.5)
    , direction = GoUp
    }
