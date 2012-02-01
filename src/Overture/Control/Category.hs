module Overture.Control.Category ( module Overture.Control.Category
                                 , module Control.Category
                                 , module Control.Arrow
                                 ) where

import Overture.ExportPrelude
-- import Overture.Data
import Control.Category
import Control.Arrow ( Arrow(..), ArrowChoice(..), Kleisli(..) )
import Prelude ()

const :: (Arrow arr) => a -> arr b a
const x = arr (\_ -> x)

swap :: (Arrow arr) => arr (a, b) (b, a)
swap = arr (\(x,y) -> (y,x))

dup :: (Arrow arr) => arr a (a, a)
dup = arr (\x -> (x,x))

fst :: (Arrow arr) => arr (a, b) a
fst = arr (\(x,_) -> x)

snd :: (Arrow arr) => arr (a, b) b
snd = arr (\(_,y) -> y)

inl :: (ArrowChoice arr) => arr a (Either a b)
inl = arr Left

inr :: (ArrowChoice arr) => arr b (Either a b)
inr = arr Right

