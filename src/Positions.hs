module Positions
  ( Config(..)
  , positionsConfig
  , gridTranslate
  , gridPosition
  ) where

import Graphics.Gloss (Picture, translate)

data Config = Config
  { windowWidth               :: Int
  , windowHeight              :: Int
  , windowXOffset             :: Int
  , windowYOffset             :: Int
  , cellSize                  :: Float
  , xMaxLimit                 :: Float
  , xMinLimit                 :: Float
  , yMaxLimit                 :: Float
  , yMinLimit                 :: Float
  , xAppleMaxLimit            :: Int
  , xAppleMinLimit            :: Int
  , yAppleMaxLimit            :: Int
  , yAppleMinLimit            :: Int
  , snakeHeadInitialX         :: Float
  , snakeHeadInitialY         :: Float
  , appleInitialX             :: Float
  , appleInitialY             :: Float
  , pointsX                   :: Float
  , pointsY                   :: Float
  , idleTimeDiminishingFactor :: Float
  , xGridOffset               :: Float
  , yGridOffset               :: Float
  }

positionsConfig :: Config
positionsConfig = Config
  { windowWidth               = 605
  , windowHeight              = 635
  , windowXOffset             = 600
  , windowYOffset             = 150
  -- tela 600 x 600, cellSize 30 => 20 x 20 celulas
  , cellSize                  = 30
  , xMaxLimit                 = 9.5 + xOffset 
  , xMinLimit                 = -9.5 + xOffset 
  , yMaxLimit                 = 9.5 + yOffset 
  , yMinLimit                 = -9.5 + yOffset 
  , xAppleMaxLimit            = 9
  , xAppleMinLimit            = -9
  , yAppleMaxLimit            = 9
  , yAppleMinLimit            = -9
  , snakeHeadInitialX         = 0.5 + xOffset 
  , snakeHeadInitialY         = 0.5 + yOffset 
  , appleInitialX             = (-0.5) + xOffset 
  , appleInitialY             = (-0.5) + yOffset 
  , pointsX                   = -295 + (xOffset * cellSize positionsConfig)
  , pointsY                   = 305 + (yOffset * cellSize positionsConfig)
  , idleTimeDiminishingFactor = 0.95 
  , xGridOffset               = xOffset 
  , yGridOffset               = yOffset 
  }

{- xOffset e yOffset definem uma forma unificada 
   de mover todos os objetos do jogo de forma
   cemtralizada. Utiliza as unidades da função 
   gridTranslate.
-}
xOffset :: Float
xOffset  = 0

yOffset :: Float
yOffset  = -0.5

-- Faz o snake andar no grid
gridTranslate :: Float -> Float -> Picture -> Picture
gridTranslate x y = translate (x * cellSize positionsConfig) (y * cellSize positionsConfig)

--posição do grid na tela
gridPosition :: Picture -> Picture
gridPosition = gridTranslate (-10 + xOffset) (-10 + yOffset)
