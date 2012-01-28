module Overture.Data ( module Overture.Data
                     , module Data.Maybe
                     , module Data.Either
                     , module Data.These
                     , module Data.Foldable
                     , module Data.Traversable
                     ) where

import Data.Either
import Data.These
import Data.Void
import Data.Maybe ( maybe, fromMaybe, isNothing, isJust
                  , catMaybes, mapMaybe, maybeToList
                  )
import Data.List ( unfoldr, intersperse, intercalate, sort
                 , sortBy, groupBy, partition
                 )
import Data.Ord (comparing)
import Data.Monoid
import Data.Semigroup
import Data.Foldable
import Data.Traversable



import Control.Applicative
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP as ReadP
import Text.Read
import qualified Prelude
import Overture.ExportPrelude


unit :: (Monoid a) => a
unit = mempty

(++) :: (Monoid a) => a -> a -> a
(++) = mappend

dup :: a -> (a, a)
dup x = (x, x)

head []    = Nothing
head (x:_) = Just x

tail []     = Nothing
tail (_:xs) = Just xs

last []     = Nothing
last [x]    = Just x
last (x:xs) = last xs

init []     = Nothing
init [x]    = Just []
init (x:xs) = (x:) <$> init xs

cycle :: [a] -> Maybe [a]
cycle [] = Nothing
cycle xs = Just $ Prelude.cycle xs

atIndex :: [a] -> Int -> Maybe a
atIndex xs n = head $ drop n xs

zipThese :: [a] -> [b] -> [These a b]
zipThese xs [] = This <$> xs
zipThese [] ys = That <$> ys
zipThese (x:xs) (y:ys) = These x y : zipThese xs ys



enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

takeUntilDiff :: (a -> a -> Bool) -> [a] -> [a]
takeUntilDiff f (x1:x2:xs) | f x1 x2   = [x1]
                           | otherwise = x1:takeUntilDiff f (x2:xs)
takeUntilDiff _ xs = xs


readEither :: (Read a) => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "no parse"
    _   -> Left "ambiguous parse"
 where
  read' = readPrec >>= \x -> lift ReadP.skipSpaces >> return x

read :: (Read a) => String -> Maybe a
read s = case readEither s of 
    Left  _ -> Nothing
    Right x -> Just x

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

justIf :: (a -> Bool) -> a -> Maybe a
justIf p x | p x 	   = Just x
           | otherwise = Nothing

rightIf :: (a -> Bool) -> (a -> Either a a)
rightIf p x = if p x then Right x else Left x

between a z x = (&&) <$> (> a) <*> (< z)


