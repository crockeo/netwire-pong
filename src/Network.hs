{-# LANGUAGE Arrows #-}
module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import Data.IORef

-------------------
-- Local Imports --
import Paddle
import Config
import Ball
import Pong

----------
-- Code --

{-|
  The wire that constructs the scene.
-}
sceneWire :: HasTime t s => Wire s () IO a Scene
sceneWire =
  pure makeScene <*> (paddle  leftUpKey  leftDownKey $ Left  ())
                 <*> (paddle rightUpKey rightDownKey $ Right ())
                 <*> (ball ballRadius)
  where makeScene :: Paddle -> Paddle -> Ball -> Scene
        makeScene p1 p2 b =
          Scene { getLeftPaddle  = p1
                , getLeftScore   = 0
                , getRightPaddle = p2
                , getRightScore  = 0
                , getBall        = b
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
