module Utils where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import Linear.V2

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
