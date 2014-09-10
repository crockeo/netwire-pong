module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
import Data.IORef

-------------------
-- Local Imports --
import Pong

----------
-- Code --

{-|
  The loop that really runs the network. Provides the input, and then renders
  the resulting @'Scene'@>
-}
runNetwork' :: HasTime t s => IORef Bool -> Session IO s -> Wire s e IO a Scene -> IO ()
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
runNetwork closedRef = print "Nothing here yet."
