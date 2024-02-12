module Interpreter where

import Utils
import Parser
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans(lift))
import System.Random
import Data.List
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe

-- je Haskell-code mag geen spelspecifieke dingen bevatten
data Game = Snake {snake :: [Coord], food :: [Coord], direction :: Direction, generator :: StdGen}
          | Tanks {tanks :: [(Coord, Direction)], bullets :: [(Coord, Direction)], gen :: StdGen, score :: Int}
          | GameOver {score :: Int, gen :: StdGen, game :: String}

-- ik ga op onderstaande lijnen geen specifieke feedback geven
-- * functies zijn veel te lang
-- * je scrapet wat data uit bestanden ipv ze te parsen
-- * algemeen zijn let/where clauses niet langer dan enkele definities
-- * ...

getValue :: (Int, [String]) -> Game -> Bool -> (Bool, Game)
getValue (index, lines) (Snake s f d g) last | last = (True, Snake newSnake f d newG)
                                             | "contains" `isInfixOf` line && sSymbol && isJust snakeContains && length s > 1 = if gameOver then (True, GameOver (length s) g "snake") else (True, Snake s f d newG)
                                             | "onBoard" `isInfixOf` line && not (onBoard (x,y)) = (True, GameOver (length s) g "snake")
                                             | "contains" `isInfixOf` line && fSymbol && isJust foodContains = if gameOver then (True, GameOver (length s) g "snake") else (True, Snake (newSnake ++ [lastCoord]) food d newG)
                                             | otherwise = (randomNumber `mod` 3 == 0, Snake newSnake ((newX,newY) : f) d newG)
                                              where line = lines !! index
                                                    value = lines !! (index + 1)
                                                    gameOver = "GameOver" `isInfixOf` value
                                                    sSymbol = ",s)" `isInfixOf` line
                                                    fSymbol = ",f)" `isInfixOf` line
                                                    x = fst (head s) + fst d
                                                    y = snd (head s) + snd d
                                                    (snakeContains, _) = getObject (runMaybeT $ contains (x,y) s) s
                                                    (foodContains, food) = getObject (runMaybeT $ contains (x,y) f) f
                                                    (randomNumber, newG) = randomR (1,10) g :: (Int,StdGen)
                                                    (lastCoord, newSnake) = getObject (addCoord (x,y)) s
                                                    ((newX, _), newY) = newCoord g
getValue (i, lines) (Tanks t b g s) last | last = (True, Tanks (newP : newTanks) movedBullets newG s) 
                                         | "contains" `isInfixOf` line && "tail" `isInfixOf` line && isJust maybeIncludes = if gameOver then (True, GameOver s newG "tanks") else (True, Tanks (newP : ((newTx,newTy),(0,1)) : [newTanks !! index1, newTanks !! index2]) [bullet | bullet <- b, not $ includes bullet $ fst $ t !! (index+1)] newG (s+1))
                                         | "contains" `isInfixOf` line && "head" `isInfixOf` line && includesAny b (fst $ head t) = if gameOver then (True, GameOver s newG "tanks") else (True, Tanks (newP : newTanks) movedBullets newG s)
                                         | "onBoard" `isInfixOf` line && not (onBoard $ fst newP) = (True, GameOver s newG "tanks")
                                         | otherwise = (isJust maybeSame, Tanks (newP : newTanks) (newBullet : movedBullets) newG s)
                                            where line = lines !! i
                                                  value = lines !! (i+1)
                                                  gameOver = "GameOver" `isInfixOf` value
                                                  ((x,y),(dx,dy)) = head t
                                                  newP = ((x+dx, y+dy), (dx,dy))
                                                  (maybeIncludes,_) = getObject (runMaybeT $ findInList (includesAny b) (tail t)) [c | (c,_) <- t]
                                                  index = fromMaybe 0 maybeIncludes
                                                  index1 = (index+1) `mod` 3
                                                  index2 = (index+2) `mod` 3
                                                  (maybeSame,_) = getObject (runMaybeT $ findInList (sameRowOrColumn (x, y)) (tail t)) [c | (c,_) <- t]
                                                  ((tx,ty),(dtx,dty)) = case maybeSame of
                                                                             Just i -> t !! (i+1)
                                                                             Nothing -> head t
                                                  newBullet = if tx == x then ((tx,ty+3* div (y-ty) (y-ty)),(0,3* div (y-ty) (y-ty))) else ((tx+3* div (x-tx) (x-tx),ty),(3* div (x-tx) (x-tx),0))
                                                  bullets = [fst bullet | bullet <- b]
                                                  (newT1, gen1) = changeCoordRandom (t !! 1) g
                                                  (newT2, gen2) = changeCoordRandom (t !! 2) gen1
                                                  (newT3, gen3) = changeCoordRandom (t !! 3) gen2
                                                  newTanks = [newT1, newT2, newT3]
                                                  ((newTx, newG), newTy) = newCoord gen3
                                                  movedBullets = [((x+dx,y+dy), (dx,dy)) | ((x,y), (dx,dy)) <-b]
getValue _ g _ = (True,g)

getPictures :: [String] -> [[Coord]] -> [Picture]
getPictures lines objects = let pictures = apply (hasSubstring "Picture" lines 0) lines
                                picLines = [l !! (i+1) | (i,l) <- pictures]
                                shapes = [drop 11 $ take i line | line <- picLines, i <- [0..(length line - 1)], line !! i == '[']
                                arguments = [drop i line | line <- picLines, i <- [0..(length line - 1)], line !! i == '[']
                            in [fst $ getObject (getPixel (shapes !! i) object) (arguments !! i) | i <- [0..(length shapes - 1)], object <- objects !! i]

checkPossibilities :: [String] -> Game -> Game
checkPossibilities lines game = let start = fst $ head $ apply (hasSubstring "next" lines 0) lines
                                    stop = fst $ head $ apply (hasSubstring "run" lines 0) lines
                                    functions = apply (possible lines start stop) lines
                                    values = [getValue f game False | f <- functions] ++ [getValue (0, lines) game True]
                                    (Just a, _) = getObject (runMaybeT $ findInList (True &&) values) [b | (b,_) <- values]
                                in snd (values !! a)

checkKey :: [String] -> SpecialKey -> Direction -> StdGen -> ([(Int,[String])], Direction, Int, StdGen)
checkKey lines key d g = let start = fst $ head $ apply (hasSubstring "move" lines 0) lines
                             stop = fst $ head $ apply (hasSubstring "next" lines 0) lines
                             possibleKeys = apply (possible lines start stop) lines
                             keyLines = [lines !! i | (i,_) <- possibleKeys]
                             dirLines = [lines !! (i+1) | (i,_) <- possibleKeys]
                             keys = [drop 7 $ take i keyLine | keyLine <- keyLines, i <- [0..(length keyLine - 1)], keyLine !! i == ')']
                             directions = [drop 10 line | line <- dirLines] ++ [show d]
                             (index,_) = getObject (indexOf (== show key)) keys
                             direction = read $ directions !! index :: Direction
                             (_, newG) = randomR(1,10) g :: (Int, StdGen)
                         in (possibleKeys, direction, index, newG)

instance Gloss Game where
  gamepic _ (GameOver s g _) = gameOverPicture s
  gamepic lines (Snake s f d g) = Pictures ([drawCoord coord (head pixels) | coord <- tail s] ++ [drawCoord (head s) (pixels !! 1)] ++ [drawCoord coord (pixels !! 2) | coord <- f])
                                    where objects = [[(0,0)],[(0,0)],[(0,0)]]
                                          pixels = getPictures lines objects
  gamepic lines (Tanks ((p,_):t) b g s) = let objects = [[p], [tank | (tank,_) <- t], [bullet | (bullet,_) <- b]] in Pictures $ getPictures lines objects
  gamepic _ _ = undefined 
  nextstep lines t (GameOver s g d) = GameOver s g d
  nextstep lines t g = checkPossibilities lines g
  nextmove lines (EventKey (SpecialKey key) Down _ _) (Snake s f d g) = let (_,direction,_,newG) = checkKey lines key d g in Snake s f direction newG 
  nextmove lines (EventKey (SpecialKey key) Down _ _) (Tanks (((x,y),(dx,dy)):t) b g s) = let (possibleKeys,_, index, newG) = checkKey lines key (dx,dy) g
                                                                                              dirLines = [lines !! (i+1) | (i,_) <- possibleKeys]
                                                                                              bullets = init [b | i <- possibleKeys] ++ [((x+3*dx,y+3*dy), (3*dx,3*dy)):b, b]
                                                                                              directions = init [drop 10 line | line <- dirLines] ++ replicate 2 (show (dx,dy))
                                                                                              newBullets = bullets !! index
                                                                                              direction = read (directions !! index) :: Direction
                                                                                          in Tanks (((x,y),direction):t) newBullets newG s 
  nextmove lines (EventKey (MouseButton  LeftButton) Down _ (x,y)) (GameOver score gen game) | restartSelected x y && snake = Snake [(0,0)] [] (0,1) gen
                                                                                             | restartSelected x y && tanks = Tanks (((0,0), (0,1)) : [((x1,y1), (0,1)), ((x2,y2), (0,1)), ((x3,y3), (0,1))]) [] gen3 0
                                                                                             | otherwise = GameOver score gen game
                                                                                                where snake = game == "snake"
                                                                                                      tanks = game == "tanks"
                                                                                                      ((x1, gen1), y1) = newCoord gen
                                                                                                      ((x2, gen2), y2) = newCoord gen1
                                                                                                      ((x3, gen3), y3) = newCoord gen2
  nextmove l e a = a


runFile :: [String] -> StdGen -> Game -> IO()
runFile lines stdGen game = play (InWindow "UGent Brick Engine" (500, 800) (10, 10))
                          white -- de achtergrondkleur
                          2 -- aantal stappen per seconde
                          game -- de beginwereld
                          (Utils.gamepic lines) -- de 'render'-functie, om naar scherm te tekenen
                          (Utils.nextmove lines) -- de 'handle'-functie, om gebruiksinvoer te verwerken
                          (Utils.nextstep lines) -- de 'step'-functie, om 1 tijdstap te laten passeren 