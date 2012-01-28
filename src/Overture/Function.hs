module Overture.Function ( module Overture.Function
                         , module Data.Function
                         ) where

import Overture.ExportPrelude
import Data.Function (fix, on)

type On a b r = (a -> b) -> r

infixl 4 ^$, ^*
(^$) :: (b -> r) -> a -> On a b r
(^$) f x g = f (g x)

(^*) :: On a b (b -> r) -> a -> On a b r
(^*) f x g = f g (g x)


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) f g = (. g) (f .)


