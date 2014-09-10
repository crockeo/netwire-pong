module Pong ( Paddle (..)
            , Ball (..)
            , Scene (..)
            , renderScene
            ) where

----------
-- Code --

{-|
  The paddle datatypes. Stores the position and the size. The score is stored
  within the @'Scene'@ datatype.
-}
data Paddle = Paddle

{-|
  The ball datatype, stores the position and the radius.
-}
data Ball = Ball

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

{-|
  Rendering a given @'Paddle'@.
-}
renderPaddle :: Paddle -> IO ()
renderPaddle _ = return ()

{-|
  Rendering a given @'Ball'@.
-}
renderBall :: Ball -> IO ()
renderBall _ = return ()

{-|
  Rendering a given scene. This includes rendering both @'Paddle'@, the
  @'Ball'@, and both of the scores.
-}
renderScene :: Scene -> IO ()
renderScene scene = do
  renderPaddle $ getLeftPaddle scene
  -- renderScore

  renderPaddle $ getRightPaddle scene
  -- renderScore

  renderBall $ getBall scene
