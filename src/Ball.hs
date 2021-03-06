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
import Collision
import Config
import Utils
import Pong

----------
-- Code --

{-|
  Determining whether or not the ball should bounce off of an edge.
-}
bounce :: V2 Float -> V2 Float -> Paddle -> Paddle -> (V2 Float, V2 Bool, V2 Bool)
bounce p@(V2 x y) (V2 w h) p1@(Paddle (V2 bx1 _) (V2 bw1 _)) p2@(Paddle (V2 bx2 _) (V2 _ _))
  | collides (Ball p ballRadius) p1 = (V2 (bx1 + bw1 + ballRadius) y, V2 True  False, V2 False False)
  | collides (Ball p ballRadius) p2 = (V2 (bx2       - ballRadius) y, V2 True  False, V2 False False)
  | x - ballRadius < (-w)           = (V2 (-w + ballRadius) y       , V2 True  False, V2 False True )
  | x + ballRadius > ( w)           = (V2 ( w - ballRadius) y       , V2 True  False, V2 True  False)
  | y - ballRadius < (-h)           = (V2 x (-h + ballRadius)       , V2 False True , V2 False False)
  | y + ballRadius > ( h)           = (V2 x ( h - ballRadius)       , V2 False True , V2 False False)
  | otherwise                       = (V2 x y                       , V2 False False, V2 False False)

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
position :: HasTime t s => Wire s e IO (Paddle, Paddle, V2 Float) (V2 Float, V2 Bool, V2 Bool)
position =
  position' (pure 0) . liftA2 (,) (mkId) (renderSize)
  where position' :: HasTime t s => V2 Float -> Wire s e IO ((Paddle, Paddle, V2 Float), V2 Float) (V2 Float, V2 Bool, V2 Bool)
        position' p =
          mkSF $ \ds ((p1, p2, dx), s) ->
            let dt             = realToFrac $ dtime ds
                (p', b, score) = bounce (p + dx * dt) s p1 p2 in
              ((p, b, score), position' p')

{-|
  Wrapping everything up into the final position.
-}
wrap :: HasTime t s => Wire s e IO (Paddle, Paddle) (V2 Float, V2 Bool)
wrap =
  proc (p1, p2) -> do
    rec s                    <- speed    -< (bx, by)
        (p, V2 bx by, score) <- position -< (p1, p2, s)
    returnA -< (p, score)

{-|
  The ball.
-}
ball :: HasTime t s => Float -> Wire s e IO (Paddle, Paddle) (Ball, V2 Bool)
ball r =
  proc ps -> do
    (pos, score) <- wrap -< ps
    returnA -< (makeBall pos, score)
  where makeBall :: V2 Float -> Ball
        makeBall p = Ball p r
