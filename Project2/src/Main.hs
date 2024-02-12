import VoorbeeldModule (hoi)
import Snake
import Tanks
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Bifunctor
import System.Random

 -- de generator "g" bij Snake wordt gebruikt bij het genereren van een willekeurig getal 
 -- en moet telkens veranderen omdat anders constant hetzelfde getal wordt gegenereerd
data Engine
    = HomeScreen
    | EndScreen {score :: Int}
    | Snake {snake :: [Coord],
             food :: [Coord],
             direction :: Direction,
             generator :: StdGen
            }
    | Tanks {player :: (Coord, Direction),
             tanks :: [(Coord, Direction)],
             bullets :: [(Coord, Direction)],
             gen :: StdGen,
             score :: Int
            }

-- Variabelen die gebruikt worden
screenGray :: Color
screenGray  = makeColorI 0x64 0x6F 0x5D 0XFF
bWidth = 250    -- lengte van een button
bHeight = 125   -- hoogte van een button
size = 20       -- grootte van de zijde van een blokje

drawCoord :: Coord -> Picture -> Picture
drawCoord (x,y) = translate (fromIntegral (size*x)) (fromIntegral (size*y))

snakeButton,tanksButton,task,restartButton :: Picture
snakeButton = Pictures [color screenGray $ rectangleSolid bWidth bHeight, translate (-75) (-20) $ scale 0.5 0.5 $ text "snake"]
tanksButton = Pictures [color screenGray $ rectangleSolid bWidth bHeight, translate (-75) (-20) $ scale 0.5 0.5 $ text "tanks"]
task = translate (-245) 250 $ scale 0.4 0.4 $ text "Choose your game"
restartButton = Pictures [color screenGray $ rectangleSolid bWidth bHeight, translate (-95) (-20) $ scale 0.5 0.5 $ text "Home"]

-- Nakijken welke knop al dan niet gelesecteerd is
snakeSelected, tanksSelected, restartSelected :: Float -> Float -> Bool
snakeSelected x y = x <= bWidth / 2 && x >= -bWidth / 2 && y <= 100 + bHeight/2 && y >= 100 - bHeight/2
tanksSelected x y = x <= bWidth / 2 && x >= -bWidth / 2 && y <= -100 + bHeight/2 && y >= -100 - bHeight/2
restartSelected x y = x <= bWidth / 2 && x >= -bWidth / 2 && y <= bHeight / 2 && y >= -bHeight / 2

-- Zet een volledig spel om in een afbeelding.
gamePic :: Engine -> Picture
gamePic HomeScreen = Pictures [translate 0 100 snakeButton, translate 0 (-100) tanksButton, task]
gamePic (EndScreen score) = let textt = "Score: " ++ show score
                            in Pictures [restartButton, translate (-250) 250 $ scale 0.8 0.8 $ text "GameOver", translate (-200) 100 $ scale 0.7 0.7 $ text textt]
gamePic (Snake s f _ _) = Pictures ([drawCoord coord snakePixel| coord <- tail s] ++ [drawCoord (head s) snakeHead] ++ [drawCoord coord foodPixel | coord <- f])
gamePic (Tanks (p,d) t b _ _) = Pictures ([tankPic black p] ++ [tankPic (greyN 0.5) tank | (tank,_) <- t] ++ [bulletPic x | (x,y) <- b])

-- Deze methode wordt op vaste intervallen aangeroepen. Laat het spel
-- een stap vooruit gaan: de kogels en obstakels verplaatsen zich
-- allemaal elk 1 tegel. (De `t` mag je voorlopig negeren.)
nextStep :: Float -> Engine -> Engine
nextStep t (Snake s f d g) | gameOver s d = EndScreen $ length s
                           | eatsFood newHead f = Snake (newHead : s) [coord | coord <- f, coord /= newHead] d newGenerator
                           | randomNumber `mod` 3 == 0 = Snake (Data.Bifunctor.bimap (x+) (y+) d : init s) (newFood x y newGenerator : f) d newGenerator
                           | otherwise = Snake (newHead : init s) f d newGenerator
                             where x = fst $ head s
                                   y = snd $ head s
                                   newHead :: Coord
                                   newHead = Data.Bifunctor.bimap (x +) (y +) d
                                   (randomNumber, newGenerator) = randomR (1,10) g :: (Int,StdGen)
nextStep _ (Tanks ((px,py), (pdx,pdy)) t b g s) | or ([hitsTank bullet $ fst $ head t | bullet <- b]) = Tanks newPlayer (newT2 : [t !! 1,t !! 2]) [bullet | bullet <- b, not $ hitsTank bullet $ fst $ head t] gen3 (s+1)
                                                | or ([hitsTank bullet $ fst $ t !! 1 | bullet <- b]) = Tanks newPlayer (newT3 : [head t,t !! 2]) [bullet | bullet <- b, not $ hitsTank bullet $ fst $ t !! 1] gen3 (s+1)
                                                | or ([hitsTank bullet $ fst $ t !! 2 | bullet <- b]) = Tanks newPlayer (newT1 : [head t,t !! 1]) [bullet | bullet <- b, not $ hitsTank bullet $ fst $ t !! 2] gen3 (s+1)
                                                | or ([hitsTank bullet (px,py) | bullet <- b]) = EndScreen s
                                                | closeTank (fst $ head t) (px,py) = Tanks newPlayer newTanks (newBullet (fst $ head t) (px,py) : movedBullets) gen3 s
                                                | closeTank (fst $ t !! 1) (px,py) = Tanks newPlayer newTanks (newBullet (fst $ t !! 1) (px,py) : movedBullets) gen3 s
                                                | closeTank (fst $ t !! 2) (px,py) = Tanks newPlayer newTanks (newBullet (fst $ t !! 2) (px,py) : movedBullets) gen3 s
                                                | otherwise = Tanks ((newX, newY), (pdx,pdy)) [newT1, newT2, newT3] movedBullets gen3 s
                                                  where newX = px+pdx
                                                        newY = py+pdy
                                                        newPlayer = ((newX,newY),(pdx,pdy))
                                                        (newT1, gen1) = newCoord (head t) g
                                                        (newT2, gen2) = newCoord (t !! 1) gen1
                                                        (newT3, gen3) = newCoord (t !! 2) gen2
                                                        newTanks = [newT1, newT2, newT3]
                                                        movedBullets = [ ((x+dx,y+dy), (dx,dy)) | ((x,y), (dx,dy)) <-b]
nextStep t engine = engine

move :: Event -> Engine -> Engine
--Gebruiktersinput bij start- en eindscherm
move (EventKey (MouseButton LeftButton) Down _ (x,y)) HomeScreen | snakeSelected x y = Snake [(0,0)] [] (0,1) (mkStdGen 5)
                                                                 | tanksSelected x y = let ((x1, gen1), y1) = createTank (mkStdGen 100)
                                                                                           ((x2, gen2), y2) = createTank gen1
                                                                                           ((x3, gen3), y3) = createTank gen2
                                                                                       in Tanks ((0,0), (0,1)) [((x1,y1), (0,1)), ((x2,y2), (0,1)), ((x3,y3), (0,1))] [] gen3 0
                                                                 | otherwise = HomeScreen
move (EventKey (MouseButton LeftButton) Down _ (x,y)) (EndScreen score) | restartSelected x y = HomeScreen
                                                                        | otherwise = EndScreen score

-- Gebruikersinput bij spel Snake
move (EventKey (SpecialKey KeyLeft) Down _ _) (Snake s f d g)   | d == (-1,0) = Snake s f d g
                                                              | otherwise = Snake s f (-1,0) g
move (EventKey (SpecialKey KeyRight ) Down _ _) (Snake s f d g) | d == (1,0) = Snake s f d g
                                                              | otherwise = Snake s f (1,0) g
move (EventKey (SpecialKey KeyUp) Down _ _) (Snake s f d g)     | d == (0,1) = Snake s f d g
                                                              | otherwise = Snake s f (0,1) g
move (EventKey (SpecialKey KeyDown ) Down _ _) (Snake s f d g)  | d == (0,-1) = Snake s f d g
                                                              | otherwise = Snake s f (0,-1) g

-- Gebruikersinput bij spel Tanks
move (EventKey (SpecialKey KeyUp) Down _ _) (Tanks (p,_) t b g s) = Tanks (p,(0,1)) t b g s
move (EventKey (SpecialKey KeyDown) Down _ _) (Tanks (p,_) t b g s) = Tanks (p,(0,-1)) t b g s
move (EventKey (SpecialKey KeyLeft) Down _ _) (Tanks (p,_) t b g s) = Tanks (p,(-1,0)) t b g s
move (EventKey (SpecialKey KeyRight) Down _ _) (Tanks (p,_) t b g s) = Tanks (p,(1,0)) t b g s
-- Bij het indrukken van de spatiebalk wordt een kogel gemaakt. Om ervoor te zorgen dat deze sneller beweegt dat de tanks, vermenigvuldig ik de richting (Direction) met 3
move (EventKey (SpecialKey KeySpace) Down _ _) (Tanks ((px,py),(x,y)) t b g s) = Tanks ((px,py),(x,y)) t (((px+3*x,py+3*y), (3*x,3*y)) : b) g s
move _ engine = engine

main :: IO ()
main  = play (InWindow "UGent Brick Engine" (500, 800) (10, 10))
             white -- de achtergrondkleur
             2 -- aantal stappen per seconde
             HomeScreen -- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             nextStep -- de 'step'-functie, om 1 tijdstap te laten passeren
