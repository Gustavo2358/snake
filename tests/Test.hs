import Test.Tasty
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Control.Monad.Reader (runReader)
import Control.Monad.State
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game
import GameMap
import KeyHandler
import Window
import Grid
import Positions
import System.Random (newStdGen, StdGen,  mkStdGen)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ QC.testProperty "Initial state is set correctly" prop_initialState
  , QC.testProperty "Direction vectors are correct" prop_directionVector
  , QC.testProperty "Collision detection is accurate" prop_isCollision
  , QC.testProperty "Game updates correctly" prop_updateGame
  , QC.testProperty "Apple is placed within limits" prop_createAppleRandomPosition
  ]

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> arbitrary

instance Arbitrary Direction where
  arbitrary = oneof $ map return [GoUp, GoDown, GoLeft, GoRight, Stop]

instance Arbitrary Game where
  arbitrary = do
    snakeHead <- arbitrary
    appleLoc <- arbitrary
    snakeColor <- arbitrary
    appleColor <- arbitrary
    snakeDirection <- arbitrary
    snakeTail <- arbitrary
    randomGen <- arbitrary
    let gameOver = False
        elapsedTime = 0
        idleTime = 0.1
    return Game { snakeHead = snakeHead
                , appleLoc = appleLoc
                , snakeColor = snakeColor
                , appleColor = appleColor
                , snakeDirection = snakeDirection
                , snakeTail = snakeTail
                , randomGen = randomGen
                , gameOver = gameOver
                , elapsedTime = elapsedTime
                , idleTime = idleTime
                , obstacles = mockedObstacleMaps
                }

instance Arbitrary Color where
  arbitrary = oneof $ map return [green, red, blue, white, black, makeColor 0 0 0 0]

mockedObstacleMaps :: ObstaclesMap
mockedObstacleMaps = 
  [
   (-9.5,9.0),(-8.5,9.0),(-7.5,9.0),(-6.5,9.0),(-3.5,9.0),(-2.5,9.0),(-1.5,9.0),
   (-0.5,9.0),(0.5,9.0),(1.5,9.0),(2.5,9.0),(3.5,9.0),(6.5,9.0),(7.5,9.0),(8.5,9.0),(9.5,9.0),
   (-9.5,8.0),(9.5,8.0),(-9.5,7.0),(-0.5,7.0),(0.5,7.0),(9.5,7.0),(-9.5,4.0),(9.5,4.0),
   (-9.5,3.0),(9.5,3.0),(-9.5,2.0),(-5.5,2.0),(-4.5,2.0),(9.5,2.0),(-9.5,1.0),(-5.5,1.0),
   (-4.5,1.0),(9.5,1.0),(-9.5,0.0),(9.5,0.0),(-9.5,-1.0),(9.5,-1.0),(-9.5,-4.0),(9.5,-4.0),
   (-9.5,-5.0),(9.5,-5.0),(-9.5,-6.0),(-4.5,-6.0),(9.5,-6.0),(-9.5,-7.0),(9.5,-7.0),
   (-9.5,-8.0),(0.5,-8.0),(9.5,-8.0),(-9.5,-9.0),(9.5,-9.0),(-9.5,-10.0),(-8.5,-10.0),
   (-7.5,-10.0),(-6.5,-10.0),(-3.5,-10.0),(-2.5,-10.0),(-1.5,-10.0),(-0.5,-10.0),(0.5,-10.0),
   (1.5,-10.0),(2.5,-10.0),(3.5,-10.0),(6.5,-10.0),(7.5,-10.0),(8.5,-10.0),(9.5,-10.0)
  ]

prop_initialState :: (Float, Float) -> StdGen -> Bool
prop_initialState randCoords gen =
  let game = runReader (initialState randCoords gen mockedObstacleMaps) positionsConfig
  in snakeHead game == (snakeHeadInitialX positionsConfig, snakeHeadInitialY positionsConfig) &&
     appleLoc game == (fst randCoords, snd randCoords) &&
     snakeColor game == green &&
     appleColor game == red &&
     snakeDirection game == Stop &&
     null (snakeTail game) &&
     not (gameOver game)

prop_directionVector :: Direction -> Bool
prop_directionVector dir =
  case dir of
    GoUp    -> directionVector dir == (0, 1)
    GoDown  -> directionVector dir == (0, -1)
    GoLeft  -> directionVector dir == (-1, 0)
    GoRight -> directionVector dir == (1, 0)
    Stop    -> directionVector dir == (0, 0)

prop_isCollision :: [(Float, Float)] -> (Float, Float) -> Bool
prop_isCollision positions pos =
  isCollision positions pos == (pos `elem` positions)

prop_updateGame :: Game -> Bool
prop_updateGame game =
  let updatedGame = runReader (execStateT (updateGame 0.001) game) positionsConfig
      newHead = runReader (calculateSnakeMovement (snakeHead game) (snakeDirection game)) positionsConfig
      newTail = calculateNewTailPosition (snakeHead game) (snakeTail game)
  in if elapsedTime game < idleTime game
     then elapsedTime updatedGame == elapsedTime game + 0.001
     else if snakeHead game == appleLoc game
          then snakeHead updatedGame == newHead &&
               snakeTail updatedGame == if null (snakeTail game) then [snakeHead game] else head (snakeTail game) : newTail
          else if isCollision newTail newHead || isCollision (obstacles game) newHead
               then gameOver updatedGame
               else snakeHead updatedGame == newHead &&
                    snakeTail updatedGame == newTail

prop_createAppleRandomPosition :: StdGen -> Property
prop_createAppleRandomPosition gen = 
  let 
      xMin = fromIntegral (xAppleMinLimit positionsConfig) + appleInitialX positionsConfig
      xMax = fromIntegral (xAppleMaxLimit positionsConfig) + appleInitialX positionsConfig
      yMin = fromIntegral (yAppleMinLimit positionsConfig) + appleInitialY positionsConfig
      yMax = fromIntegral (yAppleMaxLimit positionsConfig) + appleInitialY positionsConfig
      ((x, y), _) = runReader (createAppleRandomPosition gen mockedObstacleMaps) positionsConfig
      xValid = x >= xMin && x <= xMax
      yValid = y >= yMin && y <= yMax
      positionInObstacles = (x, y) `elem` mockedObstacleMaps
  in 
      counterexample 
        ("Generated position: (" ++ show x ++ ", " ++ show y ++ ")" ++
         "\nX limits: " ++ show xMin ++ " to " ++ show xMax ++
         "\nY limits: " ++ show yMin ++ " to " ++ show yMax ++
         (if positionInObstacles
          then "\nThe generated position is present in the obstacle map!"
          else "\nThe generated position is NOT present in the obstacle map.")) 
        (xValid && yValid && not positionInObstacles)
