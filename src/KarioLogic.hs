{-# LANGUAGE NamedFieldPuns #-}
module KarioLogic(stepKario, karioInput) where

import Model
import Collision
import Movable
import Accelerable

--movement modifiers
karioSpeed :: Float
karioSpeed = 75
karioJumpStrength :: Float
karioJumpStrength = 300
karioGroundFriction :: Float
karioGroundFriction = 20
karioAirFriction :: Float
karioAirFriction = 3

--applies all the functions to kario that make up a step
stepKario :: Float -> [Platform] -> Kario -> Kario
stepKario secs platforms kario@Kario{karHitbox = Hitbox{pos = prevPos}} =
  (handleKarioPlatformCollisions prevPos platforms . move secs . applyFrictionToKario secs . accelerateKario secs . applyGravity secs) kario 

-- handles the collision between kario and all the platforms. Determines whether collisions are horizontal or vertical and acts accordingly.
handleKarioPlatformCollisions :: Position -> [Platform] -> Kario -> Kario
handleKarioPlatformCollisions (prevX, prevY) platforms movedKario = foldr handleKarioPlatformCollision movedKario{airborne = Falling} $ getOverlaps movedKario platforms
  where
    handleKarioPlatformCollision :: Hitbox -> Kario -> Kario
    handleKarioPlatformCollision Hitbox{width = overlapX, height = overlapY} kario@Kario{karHitbox = hitbox@Hitbox{pos = (newX, newY)}, karVel = (vx, vy)}
      | overlapX < overlapY = kario{karHitbox = hitbox{pos = (prevX, newY)}, karVel = (0, vy)}                      --when the horizontal overlap is smaller we treat the collision as a horizontal one
      | vy > 0              = kario{karHitbox = hitbox{pos = (newX, prevY)}, karVel = (vx, 0)}                      --if it is not horizontal and there is upwards velocity kario bumps its head
      | otherwise           = kario{karHitbox = hitbox{pos = (newX, prevY)}, karVel = (vx, 0), airborne = Grounded} --otherwise we treat it as a vertical collision where kario falls

--moves kario using its current velocity
moveKario :: Float -> Kario -> Kario
moveKario secs kario@Kario{karHitbox = hitbox@Hitbox{pos = (px, py)}, karVel = (vx, vy)} = kario{karHitbox = hitbox{pos = (px + secs * vx, py + secs * vy)}}

-- !! currently not in use !! Adds velocity to kario based on its acceleration
accelerateKario :: Float -> Kario -> Kario
accelerateKario secs kario@Kario{karAccel = (accX, accY), karVel = (velX, velY)} = kario{karVel = (velX + secs * accX, velY + secs *accY)}

--adds the right amount of velocity to kario based on its desired velocity and the current friction
applyFrictionToKario :: Float -> Kario -> Kario
applyFrictionToKario secs kario@Kario{karVel = (velX, velY), airborne, desiredHorizontalVelocity} = kario{karVel = (velX + (desiredHorizontalVelocity - velX) * (secs * friction airborne), velY)}
  where
    friction Grounded = karioGroundFriction
    friction _        = karioAirFriction

-----------------------------------------------------------------------------------------------------------------------------------
--{INPUT}

--should probably find another way of doing this as handling all combinations quickly becomes impossible with more possible inputs
karioInput :: Inputs -> Kario -> Kario
karioInput inputState kario@Kario{karVel = (velX, velY), airborne}
  | aPressed && dPressed && jump = kario{desiredHorizontalVelocity = 0, karVel = (velX, velY + karioJumpStrength), airborne = Rising}
  | aPressed && dPressed         = kario{desiredHorizontalVelocity = 0}
  | aPressed && jump             = kario{desiredHorizontalVelocity = -karioSpeed, karVel = (velX, velY + karioJumpStrength), airborne = Rising}
  | dPressed && jump             = kario{desiredHorizontalVelocity = karioSpeed, karVel = (velX, velY + karioJumpStrength), airborne = Rising}
  | aPressed                     = kario{desiredHorizontalVelocity = -karioSpeed}
  | dPressed                     = kario{desiredHorizontalVelocity = karioSpeed}
  | jump                         = kario{karVel = (velX, velY + karioJumpStrength), airborne = Rising}
  | otherwise                    = kario{desiredHorizontalVelocity = 0}
    where
      aPressed = 'a' `elem` inputState
      dPressed = 'd' `elem` inputState
      jump = 'w' `elem` inputState && airborne == Grounded