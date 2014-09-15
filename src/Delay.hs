{-|
  Delaying the game before the start so that players have the chance to get
  ready and situated for the ball.
-}
module Delay where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import Renderable
import Config

----------
-- Code --

{-|
  The delay datatype itself.
-}
newtype Delay = Delay Float

{-|
  The @'Renderable''@ datatype.
-}
instance Renderable' Delay where
  render (Delay f) =
    renderPrimitive Quads $
      mapM_ linearVertex $
        generateVertecies (V2 (   -f * waitSize) 0)
                          (V2 (2 * f * waitSize) scoreHeight)

{-|
  Delaying before the game starts.
-}
delayStart :: HasTime t s => Float -> Wire s () IO a Renderable
delayStart u =
  mkPure $ \ds _ ->
    let dt = realToFrac $ dtime ds
        u' = u - dt in
      if u' < 0
        then (Left ()                     , delayStart 0 )
        else (Right $ Renderable $ Delay u, delayStart u')
