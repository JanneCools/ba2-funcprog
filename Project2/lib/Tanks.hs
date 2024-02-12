module Tanks where

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import Snake(Coord, Direction)
import System.Random


tankPic :: Color -> Coord -> Picture 
tankPic c (x,y) = Pictures [translate (fromIntegral (20*a)) (fromIntegral (20*b)) $ color c $ rectangleSolid 20 20 | a <- [(-1+x)..(1+x)], b <- [(-1+y)..(1+y)]]

bulletPic :: Coord -> Picture 
bulletPic (x,y) = translate (fromIntegral (20*x)) (fromIntegral (20*y)) $ color black $ circle 10.0

-- AI-tanken maken
createTank :: StdGen -> ((Int, StdGen), Int) 
createTank stdGen = (randomR (-12,12) stdGen :: (Int,StdGen), fst (randomR (-20,20) stdGen :: (Int,StdGen)))

-- nakijken of de AI-tank zich nog binnen de grenzen bevindt
onBoard :: Coord -> Bool 
onBoard (x,y) = x >= -12 && x <= 12 && y >= -20 && y <= 20

-- Een willekeurige richting kiezen voor de AI-tanken en deze 1 stap in die richting laten verplaatsen 
-- (de huidige richting waar de tank naar kijkt heeft een grotere kans om terug de nieuwe richting te worden)
-- De coördinaten van die nieuwe tank, samen met de (nieuwe) richting wordt teruggegeven in een tuple met de nieuwe StdGen
newCoord :: (Coord, Direction) -> StdGen -> ((Coord, Direction), StdGen)
newCoord ((x,y), (dx,dy)) gen | even a && onBoard (x+dx,y+dy) = (((x+dx, y+dy), (dx,dy)), newGen)
                              | a == 1 && onBoard (x,y+1) = (((x,y+1), (0,1)), newGen)
                              | a == 3 && onBoard (x+1,y) = (((x+1,y), (1,0)), newGen)
                              | a == 5 && onBoard (x,y-1) = (((x,y-1), (0,-1)), newGen)
                              | a == 7 && onBoard(x-1,y) = (((x-1,y), (-1,0)), newGen)
                              | dx /= 0 || dy /= 1 = (((x, y+1), (0,1)), newGen)
                              | otherwise = (((x,y-1), (0,-1)), newGen)
                                where (a, newGen) = randomR (1,8) gen :: (Int,StdGen)

-- Nakijken of een kogel een tank raakt
hitsTank :: (Coord, Direction) -> Coord -> Bool 
hitsTank ((bx,by),(x,y)) (tx,ty) = (bx+x) `elem` [tx-1, tx, tx+1] && (by+y) `elem` [ty-1, ty, ty+1]

-- Nakijken voor een AI-tank of de speler zich op dezelfde kolom of rij bevindt
closeTank :: Coord -> Coord -> Bool 
closeTank (x,y) (px,py) = x == px || y == py

-- Nieuwe kogel schieten vanuit AI-tank in richting van spelertank
newBullet :: Coord -> Coord -> (Coord, Direction)
newBullet (tx,ty) (px,py) | tx == px && ty >= py = ((tx, ty-3), (0,-3))
                          | tx == px && ty <= py = ((tx,ty+3), (0,3))
                          | ty == py && tx >= px = ((tx-3,ty), (-3,0))
                          | otherwise = ((tx+3,ty), (3,0))