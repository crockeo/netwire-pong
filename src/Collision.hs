{-|
  This module provides the ability for the collision of any two objects to be
  reasoned about easily.
-}
module Collision ( CollisionRectangle (..)
                 , Collidable (..)
                 , collides
                 , uncurriedCollides
                 ) where

--------------------
-- Global Imports --
import Linear.V2

----------
-- Code --

{-|
  The basic datatype to reason about collisions. Rectangles are used because
  collision detection is quick and easy to implement.
-}
data CollisionRectangle = CollisionRectangle (V2 Float) (V2 Float)

instance Collidable CollisionRectangle where
  collisionRectangle cr = cr

{-|
  Getting the location of the edges of a CollisionRectangle.
-}
topSide, bottomSide, leftSide, rightSide :: CollisionRectangle -> Float
topSide    (CollisionRectangle (V2 _ y) (V2 _ h)) = y + h
bottomSide (CollisionRectangle (V2 _ y) _       ) = y
leftSide   (CollisionRectangle (V2 x _) _       ) = x
rightSide  (CollisionRectangle (V2 x _) (V2 w _)) = x + w

{-|
  Checking if two collision rectangles intersect.
-}
collides' :: CollisionRectangle -> CollisionRectangle -> Bool
collides' c1 c2 =
  collidesVertical && collidesHorizontal
  where collidesVertical   = topSide   c1 > bottomSide c2 && bottomSide c1 < topSide   c2
        collidesHorizontal = rightSide c1 > leftSide   c2 && leftSide   c1 < rightSide c2

{-|
  A class for turning a given object into a CollisionRectangle.
-}
class Collidable a where
  collisionRectangle :: a -> CollisionRectangle

{-|
  Checking if two collidables intersect.
-}
collides :: (Collidable a, Collidable b) => a -> b -> Bool
collides c1 c2 =
  collisionRectangle c1 `collides'` collisionRectangle c2

{-|
  An uncurried version of collides.
-}
uncurriedCollides :: (Collidable a, Collidable b) => (a, b) -> Bool
uncurriedCollides (c1, c2) =
  collisionRectangle c1 `collides'` collisionRectangle c2
