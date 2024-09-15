module GameMap 
 ( ObstaclesMap()
 , loadMap 
 )
where 

import Positions

type ObstaclesMap = [(Float, Float)]

loadMap :: FilePath -> IO [(Float, Float)]
loadMap filePath = do
  content <- readFile filePath
  let linesOfFile = lines content
      obstaclePositions = concatMap parseLine (zip [0..] linesOfFile)
  return obstaclePositions

parseLine :: (Int, String) -> [(Float, Float)]
parseLine (y, line) = [ positionFromIndices (y, x) | (x, c) <- zip [0..] line, c == 'X' ]

positionFromIndices :: (Int, Int) -> (Float, Float)
positionFromIndices (y, x) = (fromIntegral x - xMaxLimit positionsConfig, yMaxLimit positionsConfig - fromIntegral y)
