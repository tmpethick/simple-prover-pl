{-# LANGUAGE FlexibleContexts #-}
module Foldable where

import Control.Monad ((<=<))
import qualified Data.Foldable as F
import qualified Data.Traversable
import Data.Functor.Foldable

cataM
  :: (Recursive t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where 
  c = f <=< mapM c <=< (return . project)
