import System.Random

import VoorbeeldModule (hoi)
import Interpreter
import Utils
import System.Environment (getArgs)



startGame :: [Char] -> StdGen -> [String] -> [String] -> IO()
startGame filename stdGen sLines tLines | filename == "Snake.cpd" = runFile sLines stdGen (Snake [(0,0)] [] (0,1) stdGen)
                                        | filename == "Tanks.cpd" = let ((x1, gen1), y1) = newCoord stdGen
                                                                        ((x2, gen2), y2) = newCoord gen1
                                                                        ((x3, gen3), y3) = newCoord gen2
                                                                    in runFile tLines stdGen (Tanks [((0,0), (0,1)), ((x1,y1), (0,1)), ((x2,y2), (0,1)), ((x3,y3), (0,1))] [] gen3 0)
                                        | otherwise = putStrLn "Wrong filename"



main :: IO ()
main = do (filename:_) <- getArgs -- het eerste argument (stack run tetris.xyz)
          stdGen <- getStdGen
          sContents <- readFile "lib/Snake.txt"
          tContents <- readFile "lib/Tanks.txt"
          startGame filename stdGen (lines sContents) (lines tContents)
