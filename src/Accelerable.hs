module Accelerable where

import Movable
import Model

class (Movable a) => Accelerable a where
    setVel :: DirectionalVelocity -> a -> a
    applyGravity :: Float -> a -> a
    applyGravity secs obj = setVel (vx, max (vy - secs * gravity) (-maxFallSpeed)) obj
      where (vx, vy) = getVel obj

instance Accelerable Kario where
    setVel vel kar = kar{karVel = vel}

instance Accelerable Enemy where
    setVel vel enemy = enemy{enemyVel = vel} 

gravity :: Float
gravity = 500

maxFallSpeed :: Float
maxFallSpeed = 300