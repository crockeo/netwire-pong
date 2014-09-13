module Utils where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import Collision

----------
-- Code --

{-|
  The current render size of the window. Useful, in this case, for checking if
  the ball should bounce off of the top or bottom. Or even to see if the
  paddles should be bounded off the top/bottom. Or to see if the ball has
  scored. The general idea is that it's ridiculously useful.

  It should be noted that the origin of the window (0, 0) is at the bottom
  left. So:

  (-w,  h)            (w,  h)
     +-----------------+
     |                 |
     |                 |
     |                 |
     +-----------------+
  (-w, -h)            (w, -h)
-}
renderSize :: Wire s e IO a (V2 Float)
renderSize =
  mkGen_ $ \_ ->
    liftA (Right . makeVector) $ get windowSize
  where makeVector :: Size -> V2 Float
        makeVector (Size w h) =
          V2 ((fromIntegral w / 640) * 100)
             ((fromIntegral h / 640) * 100)

{-|
  Checking if the user has pressed a key down. Blocks when released, produces
  the input value when pressed.
-}
isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a a
isKeyDown k =
  mkGen_ $ \a -> do
    state <- getKey k
    return $ case state of
      Release -> Left mempty
      Press   -> Right a

{-|
  Getting the top bound of the screen.
-}
topBound :: Wire s e IO a CollisionRectangle
topBound =
  mkSF_ topBound' . renderSize
  where topBound' :: V2 Float -> CollisionRectangle
        topBound' (V2 w h) =
          CollisionRectangle (V2 (-w) h)
                             (V2 (2 * w) h)

{-|
  Colliding on the top of the screen.
-}
topCollision :: Collidable a => Wire s e IO a Bool
topCollision = mkSF_ uncurriedCollides . liftA2 (,) mkId topBound

{-|
  Getting the bottom bound of the screen.
-}
bottomBound :: Wire s e IO a CollisionRectangle
bottomBound =
  mkSF_ bottomBound' . renderSize
  where bottomBound' :: V2 Float -> CollisionRectangle
        bottomBound' (V2 w h) =
          CollisionRectangle (V2 (-w) (-h * 2))
                             (V2 (2 * w) h)

{-|
  Colliding on the bottom of the screen.
-}
bottomCollision :: Collidable a => Wire s e IO a Bool
bottomCollision = mkSF_ uncurriedCollides . liftA2 (,) mkId bottomBound

{-|
  Getting the left bound of the screen.
-}
leftBound :: Wire s e IO a CollisionRectangle
leftBound =
  mkSF_ leftBound' . renderSize
  where leftBound' :: V2 Float -> CollisionRectangle
        leftBound' (V2 w h) =
          CollisionRectangle (V2 (-w * 2) (-h))
                             (V2 w (2 * h))

{-|
  Colliding on the left of the screen.
-}
leftCollision :: Collidable a => Wire s e IO a Bool
leftCollision = mkSF_ uncurriedCollides . liftA2 (,) mkId leftBound

{-|
  Getting the right bound of the screen.
-}
rightBound :: Wire s e IO a CollisionRectangle
rightBound =
  mkSF_ rightBound' . renderSize
  where rightBound' :: V2 Float -> CollisionRectangle
        rightBound' (V2 w h) =
          CollisionRectangle (V2 w (-h))
                             (V2 w (2 * h))

{-|
  Colliding on the right of the screen.
-}
rightCollision :: Collidable a => Wire s e IO a Bool
rightCollision = mkSF_ uncurriedCollides . liftA2 (,) mkId rightBound
