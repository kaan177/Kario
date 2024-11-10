{-# LANGUAGE InstanceSigs #-}
module Accelerable where

import Movable
import Model

class (Movable a) => Accelerable a where
    updateVel :: DirectionalVelocity -> a -> a
    applyGravity :: Float -> a -> a
    applyGravity secs obj = updateVel (vx, max (vy - secs * gravity) (-maxFallSpeed)) obj
      where (vx, vy) = getVel obj

instance Accelerable Camera where
    updateVel newVel (Camera box _) = Camera box newVel

instance Accelerable Kario where
    updateVel :: DirectionalVelocity -> Kario -> Kario
    updateVel vel kar = kar{karVel = vel}

instance Accelerable Enemy where
    updateVel vel enemy = enemy{enemyVel = vel} 

instance Accelerable PowerUp where 
    updateVel vel m@Mushroom{} = m{powerUpvel = vel}
    updateVel vel s@Star{}= s{powerUpvel = vel}



gravity :: Float
gravity = 500

maxFallSpeed :: Float
maxFallSpeed = 300