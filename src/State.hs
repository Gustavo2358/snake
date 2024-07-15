module State(GameState, initialState, snakeLoc, appleLoc, direction) where 

data GameState = Snake
  { snakeLoc :: (Float, Float)
  , appleLoc :: (Float, Float)
  , direction :: (Float, Float)
  }
  deriving (Show)

initialState :: GameState
initialState =
  Snake
    { snakeLoc  =  (0.5, 0.5)
    , appleLoc  = (-0.5, -0.5)
    , direction = (0, 2)
    }
