module Positions
  ( gridTranslate
  , gridPosition
  , xMaxLimit
  , xMinLimit
  , yMaxLimit
  , yMinLimit
  , xAppleMaxLimit
  , xAppleMinLimit
  , yAppleMaxLimit
  , yAppleMinLimit
  , snakeHeadInitialX
  , snakeHeadInitialY
  , appleInitialX
  , appleInitialY
  , cellSize
  , windowHeight
  , windowWidth
  , windowXOffset
  , windowYOffset
  , pointsX
  , pointsY
  ) where
import Graphics.Gloss (Picture, translate)

windowWidth, windowHeight, windowXOffset, windowYOffset :: Int
windowWidth = 605
windowHeight = 635
windowXOffset = 600
windowYOffset = 150

{- xOffset e yOffset definem uma forma unificada 
   de mover todos os objetos do jogo de forma
   cemtralizada. Utiliza as unidades da função 
   gridTranslate.
-}
xOffset :: Float
xOffset  = 0

yOffset :: Float
yOffset  = -0.5

-- tela 600 x 600, cellSize 30 => 20 x 20 celulas
cellSize :: Float
cellSize = 30

-- Faz o snake andar no grid
gridTranslate :: Float -> Float -> Picture -> Picture
gridTranslate x y = translate (x * cellSize) (y * cellSize)

--posição do grid na tela
gridPosition :: Picture -> Picture
gridPosition = gridTranslate (-10 + xOffset) (-10 + yOffset)

xMaxLimit, xMinLimit, yMaxLimit, yMinLimit :: Float
xMaxLimit = 9.5 + xOffset
xMinLimit = -9.5 + xOffset
yMaxLimit = 9.5 + yOffset
yMinLimit = -9.5 + yOffset

xAppleMaxLimit, xAppleMinLimit, yAppleMaxLimit, yAppleMinLimit :: Int
xAppleMaxLimit = 9 
xAppleMinLimit = -9 
yAppleMaxLimit = 9 
yAppleMinLimit = -9 

snakeHeadInitialX, snakeHeadInitialY :: Float
snakeHeadInitialX = 0.5 + xOffset
snakeHeadInitialY = 0.5 + yOffset

appleInitialX, appleInitialY :: Float
appleInitialX = (-0.5) + xOffset
appleInitialY = (-0.5) + yOffset

pointsX, pointsY :: Float
pointsX = -295 + xOffset * cellSize
pointsY = 305 + yOffset * cellSize