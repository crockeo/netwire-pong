{-|
  This module provides the ability for one to determine the state of the
  ball at a given time within the scope of the Netwire network.
-}

{-# LANGUAGE Arrows #-}
module Ball (ball) where

--------------------
-- Global Imports --
import Prelude hiding ((.))
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import Config
import Utils
import Pong

----------
-- Code --

{-|
  Determining whether or not the ball should bounce off of an edge.
-}
bounce :: V2 Float -> V2 Float -> (V2 Float, V2 Bool)
bounce (V2 x y) (V2 w h)
  | x - ballRadius < (-w) = (V2 (-w + ballRadius) y, V2 True False)
  | x + ballRadius > ( w) = (V2 ( w - ballRadius) y, V2 True False)
  | y - ballRadius < (-h) = (V2 x (-h + ballRadius), V2 False True)
  | y + ballRadius > ( h) = (V2 x ( h - ballRadius), V2 False True)
  | otherwise             = (V2 x y                , V2 False False)

{-|
  The speed of the ball.
-}
speed :: Monoid s => Wire s e IO (Bool, Bool) (V2 Float)
speed =
  speed' $ V2 ( ballSpeed) (-ballSpeed)
  where speed' :: Monoid s => V2 Float -> Wire s e m (Bool, Bool) (V2 Float)
        speed' s =
          mkSFN $ \(bx, by) ->
            let s' = revX bx $ revY by s in
              (s, speed' s')
          where revX :: Bool -> V2 Float -> V2 Float
                revX False s'       = s'
                revX True  (V2 x y) = V2 (-x) y

                revY :: Bool -> V2 Float -> V2 Float
                revY False s'       = s'
                revY True  (V2 x y) = V2 x (-y)

{-|
  The position of the ball.
-}
position :: HasTime t s => Wire s e IO (V2 Float) (V2 Float, Bool, Bool)
position =
  position' (pure 0) . liftA2 (,) (mkId) (renderSize)
  where position' :: HasTime t s => V2 Float -> Wire s e IO (V2 Float, V2 Float) (V2 Float, Bool, Bool)
        position' p =
          mkSF $ \ds (dx, s) ->
            let dt             = realToFrac $ dtime ds
                (p', V2 bx by) = bounce (p + dx * dt) s in
              ((p, bx, by), position' p')

{-|
  Wrapping everything up into the final position.
-}
wrap :: HasTime t s => Wire s e IO a (V2 Float)
wrap =
  proc _ -> do
    rec s           <- speed    -< (bx, by)
        (p, bx, by) <- position -< s
    returnA -< p

{-|
  The ball.
-}
ball :: HasTime t s => Float -> Wire s e IO a Ball
ball r =
  fmap makeBall wrap
  where makeBall :: V2 Float -> Ball
        makeBall p = Ball p r
