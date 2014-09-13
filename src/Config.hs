{-|
  This module provides global configuration flags, such as window width and
  height, the acceleration and deceleration speeds of paddles, and others.
-}
module Config where

-------------------
-- Global Config --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW

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
  The paddle width.
-}
paddleWidth :: Float
paddleWidth = 5

{-|
  The paddle height.
-}
paddleHeight :: Float
paddleHeight = paddleWidth * 4

{-|
  The margin of the paddle from the edge of the screen.
-}
paddleMargin :: Float
paddleMargin = paddleWidth * 2

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
minSpeed = 3

{-|
  The maximum speed of a given paddle.
-}
maxSpeed :: Float
maxSpeed = 400

{-|
  The key for going up on the left paddle.
-}
leftUpKey :: Key
leftUpKey = CharKey 'W'

{-|
  The key for going down on the left paddle.
-}
leftDownKey :: Key
leftDownKey = CharKey 'S'

{-|
  The key for going up on the right paddle.
-}
rightUpKey :: Key
rightUpKey = SpecialKey UP

{-|
  The key for going down on the right paddle.
-}
rightDownKey :: Key
rightDownKey = SpecialKey DOWN

{-|
  The render detail of the ball. The higher the value, the more triangle fans
  are rendered to complete the circle. A minimum value of 4 (I think) is
  required.
-}
renderDetail :: Float
renderDetail = 8
