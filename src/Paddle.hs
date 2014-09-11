{-|
  This modules provides functions to determine the position of a given paddle
  at a given point in time within the network.
-}

{-# LANGUAGE Arrows #-}
module Paddle (paddlePosition) where

--------------------
-- Global Imports --
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire

-------------------
-- Local Imports --
import Config
import Utils

{-|
  Applying deceleration to a paddle.
-}
decel :: Float -> Float
decel v =
  if v == 0
    then 0
    else v - (decelSpeed * signum v)

{-|
  A helper function to make applying acceleration to a paddle work smoother.
-}
accel :: Float -> Float -> Float
accel speed v =
  if signum speed /= signum v
    then speed + decelSpeed * signum speed
    else speed

{-|
  Getting the current acceleration of a paddle based on the two inputs (k1, and
  k2) that are held down.
-}
acceleration :: (Enum k, Monoid e) => k -> k -> Wire s e IO Float Float
acceleration k1 k2  =  mkSF_  decel                . isKeyDown k1 . isKeyDown k2
                   <|> mkSF_ (accel ( accelSpeed)) . isKeyDown k1
                   <|> mkSF_ (accel (-accelSpeed)) . isKeyDown k2
                   <|> mkSF_  decel

{-|
  Pushing the velocity to 0 if the velocity is small enough.
-}
stop :: Float -> Float
stop v
  | v > (-minSpeed) && v < ( minSpeed) = 0
  | otherwise                          = v

{-|
  Getting the current velocity of a paddle.
-}
velocity :: (HasTime t s, Monoid e) => Wire s e IO Float Float
velocity = mkSF_ stop . integral 0

{-|
  Getting the paddle position.
-}
paddlePosition :: (Enum k, HasTime t s, Monoid e) => k -> k -> Wire s e IO a Float
paddlePosition k1 k2 =
  proc _ -> do
    rec a <- acceleration k1 k2 -< v
        v <- velocity           -< a
        p <- integral 0         -< v

    returnA -< p
