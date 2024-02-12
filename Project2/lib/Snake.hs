module Snake where
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import System.Random

type Coord = (Int,Int)
type Direction = Coord

snakePixel, snakeHead :: Picture
snakePixel = color (makeColorI 135 211 124 255) $ rectangleSolid 20 20
snakeHead = color (makeColorI 30 130 76 255) $ rectangleSolid 20 20

foodPixel :: Picture
foodPixel = color black $ rectangleSolid 20 20

hitsEdge :: Coord -> Bool
hitsEdge (x,y) = x < -12 || x > 12 || y < -20 || y > 20

-- Controleren of de slang tegen de rand of zichzelf botst
gameOver :: [Coord] -> Direction -> Bool
gameOver s (x,y) = let newX = fst (head s) + x
                       newY = snd (head s) + y
                       newCoord = (newX,newY)
                   in [mutualCoord | mutualCoord <- s, mutualCoord == newCoord] == [newCoord] || hitsEdge (newX, newY)

-- Een willekeurige locatie meegeven voor het nieuwe eten
newFood :: Int -> Int -> StdGen -> Coord
newFood x y stdGen = (fst (randomR (-12,12) stdGen :: (Int,StdGen)), fst (randomR (-20,20) stdGen :: (Int,StdGen)))

-- Controleren of de slang eten tegenkomt en hierdoor vergroot. Het eerste argument dat je meegeeft, is de coördinaten van het hoofd van de slang
eatsFood :: Coord -> [Coord] -> Bool
eatsFood head food = not (null ([coord | coord <- food, coord == head]))