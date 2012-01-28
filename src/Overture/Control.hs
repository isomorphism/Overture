module Overture.Control ( module Overture.Control
                        , module Control.Category
                        , module Control.Arrow
                        , module Control.Applicative
                        , module Control.Monad
                        , module Control.Monad.Instances
                        , module Data.Functor
                        , module Data.Foldable
                        , module Data.Traversable
                        ) where

import Overture.Data

import Control.Category
import Control.Arrow (Kleisli(..))

import Control.Applicative ( Applicative(..), liftA2, liftA3
                           , Alternative(..)
                           )

import Control.Monad ( Monad(..), (=<<), (<=<), (>=>), join
                     , liftM, liftM2, liftM3, liftM4
                     , guard, unless, when
                     , replicateM, void
                     , MonadPlus(..)
                     )
import Control.Monad.Instances

import Data.Functor ( (<$>) )

import Data.Foldable hiding (foldl1, foldr1)
import Data.Traversable

import Data.Monoid (Monoid(..))
import Overture.ExportPrelude

bind :: (Monad m) => (a -> m b) -> (m a -> m b)
bind = (=<<)

ap :: (Applicative f) => f (a -> b) -> (f a -> f b)
ap = (<*>)

instance (Monoid w) => Monad ((,) w) where
    return = (,) mempty
    (w, x) >>= k = let (w', y) = k x
                   in (w `mappend` w', y)
    (w1, _) >> (w2, x) = (w1 `mappend` w2, x)


ignore :: (Monad m) => m b -> a -> m a
ignore m x = m >> return x

whenJust :: (Monad m) => (a -> m b) -> Maybe a -> m ()
whenJust _ Nothing = return ()
whenJust f (Just x) = f x >> return ()

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x | p x       = return x
             | otherwise = untilM p f =<< f x

unfoldM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m [a]
unfoldM f b = do x <- f b
                 case x of Just (a, b') -> liftM (a:) (unfoldM f b')
                           Nothing      -> return []

iterM :: (Monad m) => m (Maybe a) -> m [a]
iterM mx = step =<< mx
  where step (Just x) = liftM (x:) (iterM mx)
        step Nothing  = return []

iterWhileM :: (Monad m) => (a -> Bool) -> m a -> m [a]
iterWhileM p mx = iterM $ liftM (justIf p) mx

iterM_ :: (Monad m) => m Bool -> m ()
iterM_ mb = step =<< mb
  where step True  = iterM_ mb
        step False = return ()

iterWhileM_ :: (Monad m) => (a -> Bool) -> m a -> m ()
iterWhileM_ p mx = iterM_ (liftM p mx)

ensure :: (MonadPlus m) => (a -> Bool) -> a -> m a
ensure p x = ignore (guard $ p x) x
