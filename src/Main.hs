module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef

-------------------
-- Local Imports --
import Network
import Config

----------
-- Code --

{-|
  Making the callback to run when the window is asking to be closed.
-}
makeWindowCloseCallback :: IORef Bool -> WindowCloseCallback
makeWindowCloseCallback closedRef = do
  writeIORef closedRef True
  return True

{-|
  Making a callback to set the window to resize properly.
-}
makeWindowSizeCallback :: WindowSizeCallback
makeWindowSizeCallback s@(Size w' h') = do
  let (w, h) = ( (fromIntegral w' / 640) * 100
               , (fromIntegral h' / 640) * 100
               )

  matrixMode $= Projection
  loadIdentity
  ortho (-w) ( w)
        (-h) ( h)
        (-1) ( 1)

  matrixMode $= Modelview 0
  viewport $= (Position 0 0, s)

{-|
  The entry point to the program.
-}
main :: IO ()
main = do
  initialize
  openWindow (Size glWindowWidth glWindowHeight) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "netwire-pong"

  closedRef <- newIORef False

  windowCloseCallback $= makeWindowCloseCallback closedRef
  windowSizeCallback  $= makeWindowSizeCallback

  runNetwork closedRef

  closeWindow
