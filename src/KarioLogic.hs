{-# language NamedFieldPuns #-}

module KarioLogic(stepKario, karioInput) where

import Model
import Collision

--movement modifiers
karioSpeed :: Float
karioSpeed = 75
karioJumpStrength :: Float
karioJumpStrength = 300
karioGroundFriction :: Float
karioGroundFriction = 20
karioAirFriction :: Float
karioAirFriction = 3
karioGravity :: Float
karioGravity = 500
karioMaxFallSpeed :: Float
karioMaxFallSpeed = 300

--applies all the functions to kario that make up a step
stepKario :: Float -> [Platform] -> Kario -> Kario
stepKario secs platforms kario@Kario{hitbox = Hitbox{pos = prevPos}} =
    ({-karioHandleVerticalVelocity .-} handleKarioPlatformCollisions prevPos platforms . moveKario secs platforms . applyFrictionToKario secs . accelerateKario secs . applyGravityToKario secs) kario

-- handles the collision between kario and all the platforms. Determines whether collisions are horizontal or vertical and acts accordingly.
handleKarioPlatformCollisions :: Position -> [Platform] -> Kario -> Kario
handleKarioPlatformCollisions (prevX, prevY) platforms movedKario = foldr handleKarioPlatformCollision movedKario{airborne = Falling} $ getOverlaps movedKario platforms
  where
    handleKarioPlatformCollision :: Hitbox -> Kario -> Kario
    handleKarioPlatformCollision Hitbox{width = overlapX, height = overlapY} kario@Kario{hitbox = hitbox@Hitbox{pos = (newX, newY)}, dirVelocity = (vx, vy)}
      | overlapX < overlapY = kario{hitbox = hitbox{pos = (prevX, newY)}, dirVelocity = (0, vy)}                      --when the horizontal overlap is smaller we treat the collision as a horizontal one
      | vy > 0              = kario{hitbox = hitbox{pos = (newX, prevY)}, dirVelocity = (vx, 0)}                      --if it is not horizontal and there is upwards velocity kario bumps its head
      | otherwise           = kario{hitbox = hitbox{pos = (newX, prevY)}, dirVelocity = (vx, 0), airborne = Grounded} --otherwise we treat it as a vertical collision where kario falls

--moves kario using its current velocity
moveKario :: Float -> [Platform] -> Kario -> Kario
moveKario secs platforms kario@Kario{hitbox = hitbox@Hitbox{pos = (px, py)}, dirVelocity = (vx, vy)} = kario{hitbox = hitbox{pos = (px + secs * vx, py + secs * vy)}}

-- !! currently not in use !! Adds velocity to kario based on its acceleration
accelerateKario :: Float -> Kario -> Kario
accelerateKario secs kario@Kario{dirAccel = (accX, accY), dirVelocity = (velX, velY)} = kario{dirVelocity = (velX + secs * accX, velY + secs *accY)}

--adds the right amount of velocity to kario based on its desired velocity and the current friction
applyFrictionToKario :: Float -> Kario -> Kario
applyFrictionToKario secs kario@Kario{dirVelocity = (velX, velY), airborne, desiredHorizontalVelocity} = kario{dirVelocity = (velX + (desiredHorizontalVelocity - velX) * (secs * friction airborne), velY)}
  where
    friction Grounded = karioGroundFriction
    friction _        = karioAirFriction

--accelerates kario by the set gravity. Does not accelerate past the max fall speed
applyGravityToKario :: Float -> Kario -> Kario
applyGravityToKario secs kario@Kario{dirVelocity = (velX, velY)} = kario{dirVelocity = (velX, max (velY - secs * karioGravity) (-karioMaxFallSpeed))}


-----------------------------------------------------------------------------------------------------------------------------------
--{INPUT}

--should probably find another way of doing this as handling all combinations quickly becomes impossible with more possible inputs
karioInput :: Inputs -> Kario -> Kario
karioInput inputState kario@Kario{dirVelocity = (velX, velY), airborne}
  | aPressed && dPressed && jump = kario{desiredHorizontalVelocity = 0, dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
  | aPressed && dPressed         = kario{desiredHorizontalVelocity = 0}
  | aPressed && jump             = kario{desiredHorizontalVelocity = -karioSpeed, dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
  | dPressed && jump             = kario{desiredHorizontalVelocity = karioSpeed, dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
  | aPressed                     = kario{desiredHorizontalVelocity = -karioSpeed}
  | dPressed                     = kario{desiredHorizontalVelocity = karioSpeed}
  | jump                         = kario{dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
  | otherwise                    = kario{desiredHorizontalVelocity = 0}
    where
      aPressed = 'a' `elem` inputState
      dPressed = 'd' `elem` inputState
      jump = 'w' `elem` inputState && airborne == Grounded