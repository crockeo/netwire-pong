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
  The delay before the game starts.
-}
startDelay :: Float
startDelay = 3

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
paddleMargin = -(paddleWidth / 2)

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
  The radius of the ball.
-}
ballRadius :: Float
ballRadius = 3

{-|
  The render detail of the ball. The higher the value, the more triangle fans
  are rendered to complete the circle. A minimum value of 4 (I think) is
  required.
-}
renderDetail :: Float
renderDetail = 8

{-|
  The speed in which the ball should travel.
-}
ballSpeed :: Float
ballSpeed = 70

{-|
  The horizontal text margin (from the left and right of the screen -- left for
  the left paddle, right for the right).
-}
horizontalScoreMargin :: Float
horizontalScoreMargin = 10

{-|
  The vertical text margin (from the top of the screen).
-}
verticalScoreMargin :: Float
verticalScoreMargin = 20

{-|
  The height of the block that displays the score.
-}
scoreHeight :: Float
scoreHeight = 5

{-|
  The multiplyer of the size for the start delay.
-}
waitSize :: Float
waitSize = 10
