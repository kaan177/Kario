{-# LANGUAGE NamedFieldPuns #-}
module Movable where

import Model
import Collision

--Type class for movement

class (Collidable a) => Movable a where
  getVel :: a -> DirectionalVelocity
  move :: Float -> a -> a
  move secs movable = updatePos movable (px + secs * vx, py + secs * vy)
    where (px,py) = getPos movable
          (vx,vy) = getVel movable
  moveX :: Float -> a -> a
  moveX secs movable = updatePos movable (px + secs * vx, py)
    where (px,py) = getPos movable
          (vx,vy) = getVel movable
  moveY :: Float -> a -> a
  moveY secs movable = updatePos movable (px, py + secs * vy)
    where (px,py) = getPos movable
          (vx,vy) = getVel movable           
instance Movable Kario where
  getVel Kario{karVel} = karVel;

instance Movable Enemy where
  getVel = enemyVel

instance Movable PowerUp where
  getVel Mushroom{powerUpvel} = powerUpvel
  getVel  Star{powerUpvel} =  powerUpvel
  
instance Movable Camera where
  getVel (Camera _ vel) = vel
