-- Een aantal imports die wel handig kunnen blijken.
import           Data.List
import           Data.Tuple
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

--------------------------------------------------------------------------------
-- Datatypes en constanten

-- Coördinaten worden opgeslagen als paar gehele getallen. We verzinnen
-- type-aliassen hiervoor om de types van onze functies leesbaar en
-- betekenisvol te maken.
type X = Int
type Y = Int
type Coord = (X, Y)

-- Een level bestaat uit een reeks vallende lijnen. Zo'n lijn stellen
-- we voor door de lijst van kolommen waar een blokje valt.
type Line = [X]

-- Een spel kan zich in 3 mogelijke staten bevinden: de speler is nog
-- bezig, de speler is verloren, of de speler heeft gewonnen. In geval
-- van de twee laatste hoeven we niets te onthouden: we zullen gewoon
-- een vast scherm tonen. Als de speler nog bezig is, bevinden zich in
-- het spel een speler (voorgesteld door diens huidige coördinaat, een
-- aantal vallende obstakels (voorgesteld door diens coördinaten, een
-- aantal afgeschoten kogels (eveneens) en de lijnen die nog moeten
-- vallen.
data Game
  = Playing { player    :: Coord
            , obstacles :: [Coord]
            , bullets   :: [Coord]
            , lines     :: [Line]
            }
  | GameOver
  | Won

-- Twee kleuren die jullie alvast krijgen - pas deze gerust aan.
screenGreen, screenGray :: Color
screenGreen = makeColorI 0x6F 0x77 0x5F 0xFF
screenGray  = makeColorI 0x64 0x6F 0x5D 0XFF

-- Enkele constanten om te gebruiken in de rest van de code. Bij het
-- quoteren kunnen wij deze veranderen om te kijken welke invloed ze
-- hebben.
width   = 10 -- de breedte van het bord
height  = 20 -- de hoogte van het bord
dblock  = 12 -- de zijde van 1 blokje (inclusief marge rondom)
dwidth  = 10 -- de zijde van 1 blokje (exclusief marge, inclusief randje)
dinner  = 7 -- de zijde van 1 blokje (enkel het zwarte stuk middenin)
fscale  = 3 -- algemene schaal van de hele tekening

--- <-----------------------> dblock
---     <---------------> dwidth
---         <-------> dinner
---     +---------------+
---     |               |
---     |   MMMMMMMMM   |
---     |   MMMMMMMMM   |
---     |   MMMMMMMMM   |
---     |               |
---     +---------------+

-- De randen van het bord, om te gebruiken in andere functies.
bottom, top :: Y
left, right :: X
bottom = - height `div` 2
top    = height `div` 2
left   = - width `div` 2
right  = width `div` 2

------------------------------------------------------------------------
-- Grafische elementen

-- Een gevulde/actieve pixel, gecentreerd rond de oorsprong
filled :: Picture
filled = let limitPolygon = dinner / 2
         in Pictures [
          color black $ polygon [(-limitPolygon, -limitPolygon), (limitPolygon, -limitPolygon), (limitPolygon, limitPolygon), (-limitPolygon, limitPolygon), (-limitPolygon, -limitPolygon)],
          color black $ line [(-dwidth, -dwidth), (dwidth, -dwidth), (dwidth, dwidth), (-dwidth, dwidth), (-dwidth, -dwidth)]
          ]

-- Een lege pixel, gecentreerd rond de oorsprong
empty :: Picture
empty = let innerCoord = dinner / 2
        in Pictures [
          color screenGray $ polygon [(-innerCoord, -innerCoord), (innerCoord, -innerCoord), (innerCoord, innerCoord), (-innerCoord, innerCoord), (-innerCoord, -innerCoord)],
          color screenGray $ line [(-dwidth, -dwidth), (dwidth, -dwidth), (dwidth, dwidth), (-dwidth, dwidth), (-dwidth, -dwidth)]]

-- Een bord met enkel lege pixels, gecentreerd rond de oorsprong
emptyBoard :: Picture
emptyBoard = let limitX = fromIntegral(width `div` 2)
                 limitY = fromIntegral(height `div` 2)
             in Pictures [ translate (x*dblock*fscale) (y*dblock*fscale) empty | x <- [-limitX..limitX], y <- [-limitY..limitY]]

-- Een gevulde/actieve pixel op de locatie aangeduid door de coördinaat.
drawCoord :: Coord -> Picture
drawCoord (x,y) = translate (fromIntegral x*dblock*fscale) (fromIntegral y*dblock*fscale) filled 

-- Zet een volledig spel om in een afbeelding.
gamePic :: Game -> Picture
gamePic (Playing p o b _) = Pictures ([translate 0.0 0.0 emptyBoard, drawCoord p] 
                                         ++ [drawCoord obstacles | obstacles <- o] ++ [drawCoord bullets | bullets <- b])
gamePic Won = Pictures ([emptyBoard] ++ [drawCoord (-2,y) | y <- [4..6]] ++ [drawCoord (2, y) | y <- [4..6]] ++ 
                        [drawCoord (x,-2) | x <- [-1,0,1]] ++ [drawCoord (x,-1) | x <- [-2,2]] ++ [drawCoord (x,0) | x <- [-3,3]])
gamePic GameOver = Pictures ([emptyBoard] ++ [drawCoord (x, 2) | x <- [2,-2]] ++ [drawCoord (x, y) | y <- [1,3], x <- [-1,-3]] 
                             ++ [drawCoord (x,y) | x <- [1,3], y <- [1,3]] ++ [drawCoord (x,-2) | x <- [-1,0,1]] 
                             ++ [drawCoord (x,-3) | x <- [-2,2]] ++ [drawCoord (x,-4) | x <- [-3,3]])

------------------------------------------------------------------------
-- Spellogica

-- Of een gegeven coördinaat op het spelbord ligt.
onBoard :: Coord -> Bool
onBoard (x,y) = let limitX = width `div` 2
                    limitY = height `div` 2
                in x >= -limitX && x <= limitX && y >= -limitY && y <= limitY

-- Of een gegeven coördinaat op de onderste rij ligt.
atBottom :: Coord -> Bool
atBottom (_,y) = y == bottom

-- Gegeven twee lijsten van coördinaten, geef deze twee lijsten terug
-- zonder de coördinaten die ze gemeenschappelijk hebben.
collide :: [Coord] -> [Coord] -> ([Coord], [Coord])
collide l1 l2 = ([l | l <- l1, l `notElem` l2], [l | l <- l2, l `notElem` l1])

moveUp :: Coord -> Coord
moveUp (x,y) = (x, y+1)

moveDown :: Coord -> Coord
moveDown (x,y) = (x, y-1)

-- Gebruik de `move` functie om de coördinaten in `moving` te verzetten.
-- Bij botsingen met de coördinaten in `static` moeten beide coördinaten
-- verwijderd worden uit de teruggegeven lijsten.
moveAndCollide :: (Coord -> Coord) -> ([Coord], [Coord]) -> ([Coord], [Coord])
moveAndCollide move (moving, static) = collide [move coord | coord <- moving] static

-- Deze methode wordt op vaste intervallen aangeroepen. Laat het spel
-- een stap vooruit gaan: de kogels en obstakels verplaatsen zich
-- allemaal elk 1 tegel. (De `t` mag je voorlopig negeren.)
next :: Float -> Game -> Game
next t (Playing p o b l) 
     | [ x | x <-  fst coordsDown, onBoard x] /= fst coordsDown = GameOver
     | null o && null l = Won
     | null l = Playing p (snd newCoords) (fst newCoords) l
     | otherwise = Playing p (snd newCoords ++ [(x - (width `div` 2), 10) | x <- head l]) (fst newCoords) (tail l)
     where coordsDown = moveAndCollide moveDown (o,b)
           newCoords = moveAndCollide moveUp (snd coordsDown, fst coordsDown)
next t game = game

-- Hulpfuncties om een getal te decrementeren of incrementeren zonder
-- een grens te overschrijden.
decBound, incBound :: (Ord a, Num a) => a -> a -> a
decBound x b = max b (x - 1)
incBound x b = min b (x + 1)

-- Verwerk gebruiksinput.
move :: Event -> Game -> Game
move (EventKey (SpecialKey KeyLeft)  Down _ _) (Playing p o b l)
     | fst p > -width `div` 2 = Playing (fst p - 1, snd p) o b l
     | otherwise = Playing p o b l
move (EventKey (SpecialKey KeyRight) Down _ _) (Playing p o b l)
     | fst p < width `div` 2 = Playing (fst p + 1, snd p) o b l
     | otherwise = Playing p o b l
move (EventKey (SpecialKey KeySpace) Down _ _) (Playing p o b l) = Playing p o (b++[(fst p, snd p + 1)]) l
move _ game = game

------------------------------------------------------------------------
-- De main-methode en speldefinities

-- Een eenvoudig level om mee te testen.
level1 :: [Line]
level1 = [[0,1,3,4,5,9,10],[],[],[],[2,3,4,5,6,7,9,10] ]

-- Een spel waarbij de speler midden onderaan start.
startGame = Playing (0,-10) [] [] level1

-- Start het spel op.
main  = play (InWindow "UGent Brick Game" (500, 800) (10, 10))
             screenGreen -- de achtergrondkleur
             2 -- aantal stappen per seconde
             startGame -- de beginwereld
             gamePic -- de 'render'-functie, om naar scherm te tekenen
             move -- de 'handle'-functie, om gebruiksinvoer te verwerken
             next -- de 'step'-functie, om 1 tijdstap te laten passeren
