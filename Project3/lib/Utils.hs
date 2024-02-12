module Utils where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

type Coord = (Int, Int)
type Direction = Coord

-- Variabelen die gebruikt worden
screenGray :: Color
screenGray  = makeColorI 0x64 0x6F 0x5D 0XFF
bWidth :: Float 
bWidth = 250.0      -- lengte van een button
bHeight :: Float 
bHeight = 125.0     -- hoogte van een button
size = 20

--Enkele functies die door Snake.hs en Tanks.hs gebruikt worden
drawCoord :: Coord -> Picture -> Picture
drawCoord (x,y) = translate (fromIntegral (size*x)) (fromIntegral (size*y))

restartButton :: Picture 
restartButton = Pictures [color screenGray $ rectangleSolid bWidth bHeight, translate (-95) (-30) $ scale 0.5 0.5 $ text "Restart"]

restartSelected :: Float -> Float -> Bool
restartSelected x y = x <= bWidth / 2 && x >= -bWidth / 2 && y <= bHeight /2 && y >= -bHeight/2

gameOverPicture :: Int -> Picture 
gameOverPicture score = let textt = "Score: " ++ show score
                        in Pictures [restartButton, translate (-250) 250 $ scale 0.8 0.8 $ text "GameOver", translate (-200) 100 $ scale 0.7 0.7 $ text textt]


-- nakijken of de coördinaat zich nog binnen de grenzen bevindt
onBoard :: Coord -> Bool
onBoard (x,y) = x >= -12 && x <= 12 && y >= -20 && y <= 20

-- nakijken of 2 coördinaten zich op dezelfde kolom of rij bevinden
sameRowOrColumn :: Coord -> Coord -> Bool
sameRowOrColumn (x,y) (ax,ay) = x == ax || y == ay

-- Een willekeurige locatie meegeven voor een nieuwe coördinaat
newCoord :: StdGen -> ((Int, StdGen), Int)
newCoord stdGen = (randomR (-12,12) stdGen :: (Int,StdGen), fst (randomR (-20,20) stdGen :: (Int,StdGen)))

includes :: (Coord, Direction) -> Coord -> Bool
includes ((bx,by),(x,y)) (tx,ty) = bx+x >= tx-1 && bx+x <= tx+1 && by+y >= ty-1 && by+y <= ty+1 

includesAny :: [(Coord,Direction)] -> Coord -> Bool
includesAny bullets tank = or ([includes bullet tank | bullet <- bullets])

-- Een willekeurige richting kiezen en de coördinaat 1 stap in die richting laten verplaatsen 
-- (de huidige richting heeft een grotere kans om terug de nieuwe richting te worden)
changeCoordRandom :: (Coord, Direction) -> StdGen -> ((Coord, Direction), StdGen)
changeCoordRandom ((x,y), (dx,dy)) gen | even a && onBoard (x+dx,y+dy) = (((x+dx, y+dy), (dx,dy)), newGen)
                                       | a == 1 && onBoard (x,y+1) = (((x,y+1), (0,1)), newGen)
                                       | a == 3 && onBoard (x+1,y) = (((x+1,y), (1,0)), newGen)
                                       | a == 5 && onBoard (x,y-1) = (((x,y-1), (0,-1)), newGen)
                                       | a == 7 && onBoard(x-1,y) = (((x-1,y), (-1,0)), newGen)
                                       | dx /= 0 || dy /= 1 = (((x, y+1), (0,1)), newGen)
                                       | otherwise = (((x,y-1), (0,-1)), newGen)
                                        where (a, newGen) = randomR (1,8) gen :: (Int,StdGen)

class Gloss a where
    gamepic :: [String] -> a -> Picture 
    nextstep :: [String] -> Float -> a -> a
    nextmove :: [String] -> Event -> a -> a