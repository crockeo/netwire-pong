{-|
  This modules provides functions to determine the position of a given paddle
  at a given point in time within the network.
-}

{-# LANGUAGE Arrows #-}
module Paddle (paddle) where

--------------------
-- Global Imports --
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
import Linear.V2

-------------------
-- Local Imports --
import Config
import Utils
import Pong

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
    then decelSpeed * signum speed
    else speed

{-|
  Bouncing the paddle off of the top of the screen.
-}
bounce :: Bool -> Float -> Float
bounce b v =
  if b
    then (-v / 2)
    else ( v)

{-|
  Applying the max speed onto a paddle.
-}
applyMax :: Float -> Float
applyMax v
  | v > ( maxSpeed) = ( maxSpeed)
  | v < (-maxSpeed) = (-maxSpeed)
  | otherwise       = v

{-|
  Pushing the velocity to 0 if the velocity is small enough.
-}
stop :: Float -> Float
stop v
  | v > (-minSpeed) && v < ( minSpeed) = 0
  | otherwise                          = v

{-|
  Checking if the paddle is above or below the screen bounds.
-}
clamp :: (Float, V2 Float) -> (Float, Bool)
clamp (p, (V2 _ h))
  | ctop      = ( h - 1 - paddleHeight, True)
  | cbot      = (-h + 1              , True)
  | otherwise = ( p               , False)
  where ctop = p + paddleHeight > ( h)
        cbot = p                < (-h)

{-|
  Getting the current acceleration of a paddle based on the two inputs (k1, and
  k2) that are held down.
-}
acceleration :: (Enum k, Monoid e) => k -> k -> Wire s e IO Float Float
acceleration k1 k2  =  mkSF_  decel                . isKeyDown k1 . isKeyDown k2
                   <|> pure ( accelSpeed) . isKeyDown k1 -- mkSF_ (accel ( accelSpeed)) . isKeyDown k1
                   <|> pure (-accelSpeed) . isKeyDown k2 -- mkSF_ (accel (-accelSpeed)) . isKeyDown k2
                   <|> mkSF_  decel

{-|
  Getting the current velocity of a paddle.
-}
velocity :: HasTime t s => Wire s e IO (Float, Bool) Float
velocity = integralWith (\b -> bounce b . applyMax . stop) 0

{-|
  Getting the current position of the paddle. It also returns a bool
  representing whether or not the paddle should bounce off of the top/bot of
  the screen.
-}
position :: HasTime t s => Wire s e IO Float (Float, Bool)
position =
  position' 0 . liftA2 (,) (mkId) (renderSize)
  where position' :: HasTime t s => Float -> Wire s e IO (Float, V2 Float) (Float, Bool)
        position' x' =
          mkPure $ \ds (v, s) ->
            let dt = realToFrac $ dtime ds
                x  = clamp (x' + v * dt, s) in
              (Right x, position' $ fst x)

{-|
  Getting the paddle position.
-}
paddlePosition :: (Enum k, HasTime t s, Monoid e) => k -> k -> Wire s e IO a Float
paddlePosition k1 k2 =
  proc _ -> do
    rec a        <- acceleration k1 k2 -< v
        v        <- velocity           -< (a, col)
        (p, col) <- position           -< v

    returnA -< p

{-|
  Constructing the paddle.
-}
paddle :: (Enum k, HasTime t s, Monoid e) => k -> k -> Float -> Wire s e IO a Paddle
paddle k1 k2 x =
  fmap constructPaddle $ paddlePosition k1 k2
  where constructPaddle :: Float -> Paddle
        constructPaddle y = Paddle (V2 x y) (V2 paddleWidth paddleHeight)
