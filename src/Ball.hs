{-|
  This module provides the ability for one to determine the state of the
  ball at a given time within the scope of the Netwire network.
-}

{-# LANGUAGE Arrows #-}
module Ball (ball) where

--------------------
-- Global Imports --
import FRP.Netwire
import Linear.V2

-------------------
-- Local Imports --
import Config
import Utils
import Pong

----------
-- Code --

{-|
  The speed of the ball.
-}


{-|
  The position of the ball.
-}
position :: (HasTime t s, Monad m) => Wire s e m (V2 Float) (V2 Float, Bool, Bool)
position = pure (pure 0, False, False)

{-|
  Wrapping everything up into the final position.
-}
wrap :: (HasTime t s, Monad m) => Wire s e m a (V2 Float)
wrap =
  proc _ -> do
    (p, bx, by) <- position -< pure 0
    returnA -< p

{-|
  The ball.
-}
ball :: (HasTime t s, Monad m) => Float -> Wire s e m a Ball
ball r =
  fmap makeBall wrap
  where makeBall :: V2 Float -> Ball
        makeBall p = Ball p r
