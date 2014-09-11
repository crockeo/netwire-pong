{-|
  This module provides global configuration flags, such as window width and
  height, the acceleration and deceleration speeds of paddles, and others.
-}
module Config where

-------------------
-- Global Config --
import Graphics.Rendering.OpenGL

----------
-- Code --

{-|
  The width of the window.
-}
windowWidth :: Int
windowWidth = 640

{-|
  The width of the window, in an OpenGL friendly format.
-}
glWindowWidth :: GLsizei
glWindowWidth = fromIntegral windowWidth

{-|
  The height of the window.
-}
windowHeight :: Int
windowHeight = 480

{-|
  The height of the window, in an OpenGL friendly format.
-}
glWindowHeight :: GLsizei
glWindowHeight = fromIntegral windowHeight

{-|
  The speed of acceleration of a given paddle.
-}
accelSpeed :: Float
accelSpeed = 200

{-|
  The speed of deceleration of a given paddle.
-}
decelSpeed :: Float
decelSpeed = 400

{-|
  The minimum speed of a given paddle.
-}
minSpeed :: Float
minSpeed = 5
