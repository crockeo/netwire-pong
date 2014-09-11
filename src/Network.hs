{-# LANGUAGE Arrows #-}
module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import Data.IORef
import Linear.V2

-------------------
-- Local Imports --
import Paddle
import Config
import Pong

----------
-- Code --

{-|
  The wire that constructs the scene.
-}
sceneWire :: HasTime t s => Wire s () IO a Scene
sceneWire =
  proc _ -> do
    p1 <- paddlePosition leftUpKey leftDownKey   -< undefined
    p2 <- paddlePosition rightUpKey rightDownKey -< undefined

    returnA -< makeScene (p1, p2)
  where makeScene :: (Float, Float) -> Scene
        makeScene (p1, p2) =
          Scene { getLeftPaddle  = Paddle (V2 (-90) p1) (V2 5 20)
                , getLeftScore   = 0
                , getRightPaddle = Paddle (V2 ( 85) p2) (V2 5 20)
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
