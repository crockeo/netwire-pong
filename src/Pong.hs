module Pong ( Paddle (..)
            , Ball (..)
            , Scene (..)
            , renderScene
            ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Linear.V2

-------------------
-- Local Imports --
import Renderable
import Collision
import Config
import Utils

----------
-- Code --

{-|
  The paddle datatypes. Stores the position and the size. The score is stored
  within the @'Scene'@ datatype.
-}
data Paddle = Paddle (V2 Float) (V2 Float)
  deriving (Eq, Show, Read)

instance Collidable Paddle where
  collisionRectangle (Paddle pos size) = CollisionRectangle pos size

{-|
  The ball datatype, stores the position and the radius.
-}
data Ball = Ball (V2 Float) Float
  deriving (Eq, Show, Read)

instance Collidable Ball where
  collisionRectangle (Ball (V2 x y) r) =
    CollisionRectangle (V2 (x - r) (y - r))
                       (V2 (r * 2) (r * 2))

{-|
  The scene datatype contains all of the information for a given frame of the
  game. It has the @'Paddle'@s, the scores, and the @'Ball'@.
-}
data Scene = Scene { getLeftPaddle  :: Paddle
                   , getLeftScore   :: Int
                   , getRightPaddle :: Paddle
                   , getRightScore  :: Int
                   , getBall        :: Ball
                   }
  deriving (Eq, Show, Read)

{-|
  Rendering a given @'Paddle'@.
-}
renderPaddle :: Paddle -> IO ()
renderPaddle (Paddle pos size) =
  renderPrimitive Quads $
    mapM_ linearVertex $ generateVertecies pos size

{-|
  Rendering a given @'Ball'@.
-}
renderBall :: Ball -> IO ()
renderBall (Ball pos r) =
  renderPrimitive TriangleFan $
    mapM_ linearVertex $ pos : gvs 0
  where gvs :: Float -> [V2 Float]
        gvs radians
          | radians > 360 = []
          | otherwise     = generateVertex : gvs (radians + (2 * pi / renderDetail))
          where generateVertex :: V2 Float
                generateVertex = pos + (V2 r r) * V2 (sin radians) (cos radians)

{-|
  Rendering a line down the middle of the screen.
-}
renderCenter :: V2 Float -> IO ()
renderCenter (V2 _ h) = do
  renderPrimitive Lines $ do
    vertex $ Vertex2 (0 :: GLfloat) (realToFrac ( h) :: GLfloat)
    vertex $ Vertex2 (0 :: GLfloat) (realToFrac (-h) :: GLfloat)

{-|
  Rendering a given score on a given side.
-}
renderScore :: Float -> Either () () -> V2 Float -> IO ()
renderScore score side (V2 w h) =
  renderPrimitive Quads $
    mapM_ linearVertex $
      case side of
        Left  () ->
          generateVertecies (V2 (-w + horizontalScoreMargin) (h - verticalScoreMargin))
                            (V2 score scoreHeight)
        Right () ->
          generateVertecies (V2 ( w - horizontalScoreMargin - score) (h - verticalScoreMargin))
                            (V2 score scoreHeight)

{-|
  Rendering a given scene. This includes rendering both @'Paddle'@, the
  @'Ball'@, and both of the scores.
-}
renderScene :: Scene -> IO ()
renderScene scene = do
  r <- renderSize'

  color $ Color3 (0.5 :: GLfloat) (0.5 :: GLfloat) (0.5 :: GLfloat)
  renderCenter r

  color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (1 :: GLfloat)
  renderPaddle $ getLeftPaddle scene
  renderPaddle $ getRightPaddle scene
  renderBall $ getBall scene

  color $ Color3 (1 :: GLfloat) (1 :: GLfloat) (0.3 :: GLfloat)
  renderScore (fromIntegral $  getLeftScore scene) (Left  ()) r
  renderScore (fromIntegral $ getRightScore scene) (Right ()) r

{-|
  The @'Renderable''@ instance for @'Scene'@.
-}
instance Renderable' Scene where
  render = renderScene
