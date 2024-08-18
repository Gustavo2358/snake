import Test.Tasty
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Game
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
    gameOver <- arbitrary
    return Game { snakeHead = snakeHead
                , appleLoc = appleLoc
                , snakeColor = snakeColor
                , appleColor = appleColor
                , snakeDirection = snakeDirection
                , snakeTail = snakeTail
                , randomGen = randomGen
                , gameOver = gameOver
                }

instance Arbitrary Color where
  arbitrary = oneof $ map return [green, red, blue, white, black, makeColor 0 0 0 0]

prop_initialState :: (Int, Int) -> StdGen -> Bool
prop_initialState randCoords gen =
  let game = initialState randCoords gen
  in snakeHead game == (snakeHeadInitialX, snakeHeadInitialY) &&
     appleLoc game == (appleInitialX + fromIntegral (fst randCoords), appleInitialY + fromIntegral (snd randCoords)) &&
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

prop_updateGame :: Float -> Game -> Bool
prop_updateGame delta game =
  let updatedGame = updateGame delta game
      newHead = calculateSnakeMovement (snakeHead game) (snakeDirection game)
      newTail = calculateNewTailPosition (snakeHead game) (snakeTail game)
  in if snakeHead game == appleLoc game
     then snakeHead updatedGame == newHead &&
          snakeTail updatedGame == if null (snakeTail game) then [snakeHead game] else head (snakeTail game) : newTail
     else if isCollision newTail newHead
          then gameOver updatedGame
          else snakeHead updatedGame == newHead &&
               snakeTail updatedGame == newTail

prop_createAppleRandomPosition :: StdGen -> Bool
prop_createAppleRandomPosition gen =
  let ((x, y), _) = createAppleRandomPosition gen
  in x >= xAppleMinLimit && x <= xAppleMaxLimit &&
     y >= yAppleMinLimit && y <= yAppleMaxLimit