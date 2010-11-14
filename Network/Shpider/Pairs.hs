-- | This module provides a nice syntax for defining a list of pairs.
{-# OPTIONS -XFlexibleInstances -XRankNTypes #-}
module Network.Shpider.Pairs 
   ( PairsWriter
   , (=:)
   , pairs
   ) where

import Control.Monad.State

-- | The abstract type describing the monadic state of a list of pairs.
type PairsWriter a b =
   State [ ( a , b ) ]

-- | Take a monadic PairsWriter and return a list of pairs.
pairs :: forall a b c. PairsWriter a b c -> [ ( a , b ) ]
pairs =
   reverse . snd . flip runState [ ]

-- | Make a list of pairs of pairs like
--
-- @
--    pairs $ do $ 3 =: ( \" is my favourite number or \" , 5 )
--                 10 =: ( \" pints have I drunk or was it \" , 11 )
-- @
(=:) :: forall a b. a -> b -> PairsWriter a b ( )
(=:) k v = do
   st <- get
   put $ ( k , v ) : st
