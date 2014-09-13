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
import Collision
import Config

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
                       (V2 (x + r) (y + r))

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
  Performing a vertex call on a @'V2'@ @'Float'@.
-}
linearVertex :: V2 Float -> IO ()
linearVertex (V2 x y) =
  vertex $ Vertex2 (realToFrac x :: GLfloat) (realToFrac y :: GLfloat)

{-|
  Rendering a given @'Paddle'@.
-}
renderPaddle :: Paddle -> IO ()
renderPaddle (Paddle pos size) =
  renderPrimitive Quads $
    mapM_ linearVertex $ generateVertecies pos size
  where generateVertecies :: V2 Float -> V2 Float -> [V2 Float]
        generateVertecies (V2 x y) (V2 w h) =
          [ V2 (x    ) (y    )
          , V2 (x + w) (y    )
          , V2 (x + w) (y + h)
          , V2 (x    ) (y + h)
          ]

{-|
  Rendering a given score on a given side.
-}
renderScore :: Int -> Either () () -> IO ()
renderScore _ _ = return ()

{-|
  Rendering a given @'Ball'@.
-}
renderBall :: Ball -> IO ()
renderBall (Ball pos r) =
  renderPrimitive TriangleFan $
    mapM_ linearVertex $ pos : generateVertecies 0
  where generateVertecies :: Float -> [V2 Float]
        generateVertecies radians
          | radians > 360 = []
          | otherwise     = generateVertex : generateVertecies (radians + (2 * pi / renderDetail))
          where generateVertex :: V2 Float
                generateVertex = pos + (V2 r r) * V2 (sin radians) (cos radians)

{-|
  Rendering a given scene. This includes rendering both @'Paddle'@, the
  @'Ball'@, and both of the scores.
-}
renderScene :: Scene -> IO ()
renderScene scene = do
  renderPaddle $ getLeftPaddle scene
  renderScore (getLeftScore scene) (Left ())

  renderPaddle $ getRightPaddle scene
  renderScore (getRightScore scene) (Right ())

  renderBall $ getBall scene
