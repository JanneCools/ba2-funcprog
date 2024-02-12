{-# LANGUAGE TupleSections #-}
module Parser where

import Utils
import Control.Monad
import Control.Monad.Trans.Class (MonadTrans(lift))
import System.Random
import Data.List
import Graphics.Gloss.Interface.Pure.Game
import qualified Data.Bifunctor


newtype Parser a = Parser ([String] -> [(a, [String])])

apply :: Parser a -> [String] -> [(a, [String])]
apply (Parser p) = p

parse :: Parser a -> [String] -> [a]
parse p s = [x | (x,s1) <- apply p s]

-- Alle indexen geven waarbij de string de gegeven substring bevat
-- dit is geen parser combinator
hasSubstring :: String -> [String] -> Int -> Parser Int
hasSubstring substring lines start = Parser f where f [] = [(length lines, lines)]
                                                    f l = [(i, l) | i <- [start..(length l-1)], isInfixOf substring $ l !! i]

-- Alle indexen zoeken tussen de gegeven start en stop index die een if-statement bevatten
-- dit is geen parser combinator
possible :: [String] -> Int -> Int -> Parser Int
possible lines start stop = Parser p
                              where indices = apply (hasSubstring "if" lines start) lines
                                    p [] = []
                                    p l = [i | i <- indices, fst i < stop]


-- dit hoort niet thuis in een parsermodule, stop dit in een Gamemodule o.i.d.
newtype Object r h = Object {getObject :: r -> (h, r)}

instance Monad (Object r) where
  return h = Object (h,)
  (Object h) >>= f = Object $ \r -> let (h1, r1) = h r
                                        (Object h2) = f h1
                                    in h2 r1

instance Applicative (Object r) where
  pure = return
  (Object h) <*> (Object i) = Object $ \r -> let (f, r1) = h r
                                                 (h1, r2) = i r
                                             in (f h1, r2)

instance Functor (Object r) where
  fmap f (Object h) = Object $ \r -> let (h1, r1) = h r
                                     in (f h1, r1)

-- wat is het nut van de Object in deze functie?
getPixel :: String -> Coord -> Object String Picture
getPixel string (x,y) = Object $ \s -> let n = read s :: [Int]
                                           size = fromIntegral(n !! 2)
                                           shape = if string == "square" then rectangleSolid size size else circle size
                                           pic = color (makeColorI (n !! 3) (n !! 4) (n !! 5) (n !! 6)) shape
                                           min = head n
                                           max = n !! 1
                                        in (Pictures [translate (fromIntegral (20*a)) (fromIntegral (20*b)) pic | a <- [(x+min)..(x+max)], b <- [(y+min)..(y+max)]],s)

-- wat is het nut van de Object in deze functie?
choose :: Eq a => a -> Object [a] a
choose c = Object $ \r -> (c, [x | x <- r, x /= c])

-- Nieuwe coord wordt vooraan toegevoegd en laatste Coord wordt apart geplaatst
-- wat is het nut van de Object in deze functie?
addCoord :: Coord -> Object [Coord] Coord
addCoord x = Object $ \r -> (last r, x:init r)

-- wat is het nut van de Object in deze functie?
indexOf :: (a -> Bool) -> Object [a] Int
indexOf f = Object $ \r -> let index = [i | i <- [0..(length r - 1)], f $ r !! i]
                           in if null index then (length r, undefined) else (head index, [r !! head index])

-- ik zie niet waarom je hier Object zelfs gedefinieerd hebt
-- het is een monad maar je gebruikt geen enkele keer deze eigenschap
-- dit voelt aan alsof je je code complexer maakte om een monad te kunnen gebruiken, maar niet wist waarvoor die nuttig was

-- je kon MaybeT ook importeren uit de stdlib
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  a >>= f = MaybeT $ do m1 <- runMaybeT a
                        maybe (return Nothing)
                              (runMaybeT . f)
                              m1

instance Monad m => Applicative (MaybeT m) where
  pure = return
  r <*> k = MaybeT $ do m1 <- runMaybeT r
                        m2 <- runMaybeT k
                        return $ m1 <*> m2

instance Monad m => Functor (MaybeT m) where
  fmap f a = MaybeT $ do m1 <- runMaybeT a
                         return $ fmap f m1

contains :: Eq a => a -> [a] -> MaybeT (Object [a]) a
contains element list | not (null intersection) = MaybeT $ Just <$> choose (head intersection)
                      | otherwise = MaybeT $ return Nothing
                        where intersection = [c | c <- list, c == element]

findInList :: (a -> Bool) -> [(a, b)] -> MaybeT (Object [a]) Int
findInList f x | null indices = MaybeT $ return Nothing
               | otherwise = MaybeT $ return (Just $ head indices)
                  where indices = [i | i <- [0..(length x - 1)], f $ fst (x !! i)]

-- de parser van de taal ontbreekt
