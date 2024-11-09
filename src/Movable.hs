{-# LANGUAGE NamedFieldPuns #-}
module Movable where

import Model
import Collision
import Positioning

--Type class for movement

class (Collidable a) => Movable a where
    getVel :: a -> DirectionalVelocity
    move :: Float -> a -> a
    move secs movable = updatePos movable (px + secs * vx, py + secs * vy)
      where (px,py) = getPos movable
            (vx,vy) = getVel movable

instance Movable Kario where
    getVel Kario{karVel} = karVel;