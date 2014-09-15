{-# LANGUAGE ExistentialQuantification #-}
module Renderable where

----------
-- Code --

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Linear.V2

{-|
  The typeclass for the datatype.
-}
class Renderable' a where
  render :: a -> IO ()

{-|
  The renderable datatype.
-}
data Renderable = forall a. Renderable' a => Renderable a

{-|
  The Renderable' instance for @'Renderable'@.
-}
instance Renderable' Renderable where
  render (Renderable a) = render a

{-|
  Performing a vertex call on a @'V2'@ @'Float'@.
-}
linearVertex :: V2 Float -> IO ()
linearVertex (V2 x y) =
  vertex $ Vertex2 (realToFrac x :: GLfloat) (realToFrac y :: GLfloat)

{-|
  Generating a list of vertecies for a rectangle from a position and a size.
-}
generateVertecies :: V2 Float -> V2 Float -> [V2 Float]
generateVertecies (V2 x y) (V2 w h) =
  [ V2 (x    ) (y    )
  , V2 (x + w) (y    )
  , V2 (x + w) (y + h)
  , V2 (x    ) (y + h)
  ]
