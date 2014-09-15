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
import Renderable
import Paddle
import Config
import Delay
import Ball
import Pong

----------
-- Code --

{-|
  Calculating the score.
-}
score :: V2 Int -> Wire s e IO (V2 Bool) (V2 Int)
score s =
  mkSFN $ \ss ->
    let s' = addIf s ss in
      (s, score s')
  where addIf :: V2 Int -> V2 Bool -> V2 Int
        addIf s' b = s' + fmap (\x -> if x then 1 else 0) b

{-|
  The wire that constructs the scene.
-}
sceneWire :: HasTime t s => Wire s () IO a Renderable
sceneWire =
  proc _ -> do
    lpaddle <- paddle  leftUpKey  leftDownKey $ Left  () -< undefined
    rpaddle <- paddle rightUpKey rightDownKey $ Right () -< undefined
    (b, s)  <- ball ballRadius                           -< (lpaddle, rpaddle)
    s'      <- score (V2 0 0)                            -< s

    returnA -< Renderable $ makeScene lpaddle rpaddle b s'
  where makeScene :: Paddle -> Paddle -> Ball -> V2 Int -> Scene
        makeScene p1 p2 b (V2 sl sr) =
          Scene { getLeftPaddle  = p1
                , getLeftScore   = sl
                , getRightPaddle = p2
                , getRightScore  = sr
                , getBall        = b
                }

{-|
  The final wire, wrapping everthing up.
-}
finalWire :: HasTime t s => Wire s () IO a Renderable
finalWire  =  delayStart startDelay --> sceneWire

{-|
  The loop that really runs the network. Provides the input, and then renders
  the resulting @'Scene'@>
-}
runNetwork' :: HasTime t s => IORef Bool -> Session IO s -> Wire s () IO a Renderable -> IO ()
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
          render scene
          swapBuffers

          runNetwork' closedRef session' wire'

{-|
  The front end of running the network. All of the grunt work is actually done
  by runNetwork'.
-}
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = runNetwork' closedRef clockSession_ finalWire
