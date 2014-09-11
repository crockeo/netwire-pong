{-# LANGUAGE Arrows #-}
module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
import Data.IORef
import Linear.V2

import Debug.Trace

-------------------
-- Local Imports --
import Utils
import Pong

----------
-- Code --


{-|
  The speed of acceleration.
-}
accelSpeed :: Float
accelSpeed = 200

{-|
  The speed of deceleration.
-}
decelSpeed :: Float
decelSpeed = 400

{-|
  The minimum speed.
-}
minSpeed :: Float
minSpeed = 1

{-|
  The acceleration of a given paddle.
-}
acceleration :: (Enum k, Monoid e) => k -> k -> Wire s e IO Float Float
acceleration upKey downKey =  mkSF_ decel                 . isKeyDown upKey . isKeyDown downKey
                          <|> mkSF_ (accel ( accelSpeed)) . isKeyDown upKey
                          <|> mkSF_ (accel (-accelSpeed)) . isKeyDown downKey
                          <|> mkSF_ decel
  where decel :: Float -> Float
        decel v
          | v <  0 = ( decelSpeed)
          | v >  0 = (-decelSpeed)
          | v == 0 = 0

        accel :: Float -> Float -> Float
        accel s v
          | signum s == signum v = s
          | otherwise            = s + (signum s * decelSpeed)

{-|
  The velocity of a given paddle.
-}
velocity :: (HasTime t s, Monad m) => Wire s e m Float Float
velocity =
  mkSF_ stop . integral 0
  where stop :: Float -> Float
        stop v
          | v > (-minSpeed)
         && v < ( minSpeed) = 0
          | otherwise = v

{-|
  The position of a given paddle.
-}
position :: HasTime t s => Wire s e m Float Float
position = integral 0

{-|
  The wire that constructs the scene.
-}
sceneWire :: HasTime t s => Wire s () IO a Scene
sceneWire =
  proc _ -> do
    rec a1 <- acceleration (CharKey 'Q') (CharKey 'A') -< v1
        v1 <- velocity -< a1
        p1 <- position -< v1

        a2 <- acceleration (CharKey 'W') (CharKey 'S') -< v2
        v2 <- velocity -< a2
        p2 <- position -< v2

    returnA -< makeScene (p1, p2)
  where makeScene :: (Float, Float) -> Scene
        makeScene (p1, p2) =
          Scene { getLeftPaddle  = Paddle (V2 0 p1) (V2 5 20)
                , getLeftScore   = 0
                , getRightPaddle = Paddle (V2 0 p2) (V2 5 20)
                , getRightScore  = 0
                , getBall        = Ball (pure 0) 0
                }

{-|
  The loop that really runs the network. Provides the input, and then renders
  the resulting @'Scene'@>
-}
runNetwork' :: HasTime t s => IORef Bool -> Session IO s -> Wire s () IO a Scene -> IO ()
runNetwork' closedRef session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st, session') <- stepSession session
      (wt, wire'   ) <- stepWire wire st $ Right undefined

      case wt of
        Left _ -> return ()
        Right scene -> do
          clear [ColorBuffer]
          renderScene scene
          swapBuffers

          runNetwork' closedRef session' wire'

{-|
  The front end of running the network. All of the grunt work is actually done
  by runNetwork'.
-}
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = runNetwork' closedRef clockSession_ sceneWire
