module Overture.Control.Functor ( module Overture.Control.Functor
                                , module Data.Functor
                                , module Data.Functor.Compose
                                , module Data.Functor.Identity
                                , module Data.Functor.Contravariant
                                , module Control.Applicative
                                , module Data.Foldable
                                , module Data.Traversable
                                ) where

import Control.Applicative ( Applicative(..), liftA2, liftA3
                           , Alternative(..)
                           )
import Data.Functor ( (<$>) )
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Functor.Contravariant (Contravariant(..))
import Data.Foldable hiding (foldl1, foldr1)
import Data.Traversable
import Overture.ExportPrelude
import Prelude ()

ap :: (Applicative f) => f (a -> b) -> (f a -> f b)
ap = (<*>)

