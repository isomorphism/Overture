module Overture.Data ( module Data.Void
                     , module Data.Maybe
                     , module Data.Either
                     , module Data.These
                     , module Data.List
                     , module Data.Ord
                     , module Data.Monoid
                     , module Data.Foldable
                     , module Data.Traversable
                     , module Data.Complex
                     , module Data.Ix
                     -- * Renamed functions
                     , unit, (++)
                     -- * Generic equivalents
                     , map, replicate, drop, take, splitAt, length
                     -- * Replacements for needlessly partial functions
                     , head, tail, last, init, cycle, atIndex
                     , read, readEither
                     -- * Assorted utility functions
                     , enumerate, takeUntil2
                     , (??), justIf
                     , between
                     , rightIf, isLeft, isRight, eitherToMaybe, fromEither
                     ) where

import Data.Either
import Data.These
import Data.Void
import Data.Maybe ( maybe, fromMaybe, isNothing, isJust
                  , catMaybes, mapMaybe, maybeToList
                  )
import Data.List ( unfoldr, intersperse, intercalate, sort
                 , sortBy, groupBy, partition, stripPrefix
                 )
import Data.Ord (comparing)
import Data.Monoid (Monoid(..))
import Data.Foldable
import Data.Traversable
import Data.Complex (Complex(..), realPart, imagPart)
import Data.Ix

import Control.Applicative
import Text.ParserCombinators.ReadPrec
import Text.ParserCombinators.ReadP as ReadP hiding (between)
import Text.Read hiding (read)
import qualified Prelude
import qualified Data.List as List
import Overture.ExportPrelude
import Prelude ()

-- better names

-- | Alias for 'mempty'
unit :: (Monoid a) => a
unit = mempty

infixr 5 ++

-- | Alias for 'mappend'
(++) :: (Monoid a) => a -> a -> a
(++) = mappend

-- | A better name for 'fmap'.
map :: (Functor f) => (a -> b) -> (f a -> f b)
map = fmap

replicate :: (Integral z) => z -> a -> [a]
replicate = List.genericReplicate

drop :: (Integral z) => z -> [a] -> [a]
drop = List.genericDrop

take :: (Integral z) => z -> [a] -> [a]
take = List.genericTake

length :: (Integral z) => [a] -> z
length = List.genericLength

splitAt :: (Integral z) => z -> [a] -> ([a], [a])
splitAt = List.genericSplitAt


head :: [a] -> Maybe a
head []    = Nothing
head (x:_) = Just x

tail :: [a] -> Maybe [a]
tail []     = Nothing
tail (_:xs) = Just xs

last :: [a] -> Maybe a
last []     = Nothing
last [x]    = Just x
last (_:xs) = last xs

init :: [a] -> Maybe [a]
init []     = Nothing
init [_]    = Just []
init (x:xs) = (x:) <$> init xs

cycle :: [a] -> Maybe [a]
cycle [] = Nothing
cycle xs = Just $ Prelude.cycle xs

atIndex :: [a] -> Integer -> Maybe a
atIndex xs n = head $ drop n xs

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

-- | A list from 'minBound' to 'maxBound'.
enumerate :: (Enum a, Bounded a) => [a]
enumerate = [minBound .. maxBound]

-- | Takes a prefix of a list until two successive elements satisfy the
--   given predicate, keeping the first.
takeUntil2 :: (a -> a -> Bool) -> [a] -> [a]
takeUntil2 f (x1:x2:xs) | f x1 x2   = [x1]
                        | otherwise = x1:takeUntil2 f (x2:xs)
takeUntil2 _ xs = xs

-- | A synonym for 'fromMaybe' with inverted arguments. Right-associative to
--   allow chaining, e.g. @x ?? y ?? z@.
(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

infixr 3 ??

-- | Keeps or discards values based on a predicate. Useful for unfolds.
justIf :: (a -> Bool) -> a -> Maybe a
justIf p x | p x 	   = Just x
           | otherwise = Nothing

-- | Encodes the result of a predicate using 'Either'.
rightIf :: (a -> Bool) -> (a -> Either a a)
rightIf p x = if p x then Right x else Left x

between :: (Ord a) => a -> a -> a -> Bool
between a z = (&&) <$> (> a) <*> (< z)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (\_ -> Nothing) Just

fromEither :: (a -> b) -> Either a b -> b
fromEither f (Left l)  = f l
fromEither _ (Right r) = r

-- | Case analysis for 'Bool'.
bool :: r -> r -> Bool -> r
bool f _ False = f
bool _ t True  = t




-- some missing instances

instance Foldable ((,) a) where
    foldr f z (_, x) = f x z

instance Traversable ((,) a) where
    sequenceA (x, y) = (,) x <$> y


