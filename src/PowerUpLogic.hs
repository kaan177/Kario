{-# LANGUAGE NamedFieldPuns #-}


module PowerUpLogic(stepPowerUp) where
import Model
import Movable
import Accelerable (Accelerable(updateVel, applyGravity))
import Collision (getOverlaps, getOverlap, Collidable (getBox), getPos, updatePos, overlapYBias, collisionFailSafe)
import Model (PowerUp(powerShouldExist))

stepPowerUp :: Float -> [Platform] -> Kario -> PowerUp -> PowerUp
stepPowerUp secs platforms kario powerUp = let prevPos = getPos powerUp in 
    moveAndCollide secs platforms 
    . applyGravity secs 
    . handleVelocity 
    . checkKarioCollison kario  
    $ powerUp

checkKarioCollison :: Kario -> PowerUp -> PowerUp
checkKarioCollison kario powerUp = case getOverlap (getBox kario) (getBox powerUp) of
    Nothing      -> powerUp
    Just overlap -> handleKarioCollision overlap kario powerUp 

handleKarioCollision :: Overlap -> Kario -> PowerUp -> PowerUp
handleKarioCollision Hitbox{width = overX, height = overY} kario = consume
    where
          consume e@Mushroom{}       = e{powerShouldExist = RemoveIn 0}
          consume e@Star {}          = e{powerShouldExist = RemoveIn 0}

--by moving over the x-axis and y-axis seperately we avoid some bugs that arose from our collision implementation 
moveAndCollide :: Float -> [Platform] -> PowerUp -> PowerUp
moveAndCollide secs platforms powerUp = foldl collisionFailSafe(handlePlatformCollisions (getPos powerUp') platforms . moveY secs $ powerUp') platforms
    where
        powerUp' = handlePlatformCollisions (getPos powerUp) platforms . moveX secs $ powerUp
        
handlePlatformCollisions :: Position -> [Platform] -> PowerUp -> PowerUp
handlePlatformCollisions prevPos platforms movedPowerUp = foldr (handleCollision prevPos) movedPowerUp $ getOverlaps movedPowerUp platforms

handleCollision :: Position -> Overlap -> PowerUp -> PowerUp
handleCollision (prevX,prevY) Hitbox{width = overlapX, height = overlapY} powerUp
    | overlapX < (overlapY - overlapYBias) = updateVel (- vx, vy) $  updatePos powerUp (prevX, newY) --when the horizontal overlap is smaller we treat the collision as a horizontal one
    | otherwise                            = updateVel (vx,0) $ updatePos powerUp (newX, prevY)      --otherwise we treat it as a vertical collision where the enemy falls
    where (newX,newY) = getPos powerUp
          (vx,vy)     = getVel powerUp

handleVelocity :: PowerUp -> PowerUp
handleVelocity powerUp = case getVel powerUp of
                         (0,0) -> setDefaultVelocity powerUp
                         _     -> powerUp

setDefaultVelocity :: PowerUp -> PowerUp
setDefaultVelocity m@Mushroom{}      = m{powerUpvel  = (-20, 0)}
setDefaultVelocity s@Star{}          = s{powerUpvel = (-20,0)}