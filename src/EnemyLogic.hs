{-# language NamedFieldPuns #-}

module EnemyLogic(stepEnemy) where

import Model
import Accelerable ( Accelerable(updateVel, applyGravity) )
import Collision
import Positioning
import Movable
import Accelerable (Accelerable(applyGravity))
import Data.IntMap (update)

--should add logic for when out of screen not move
stepEnemy :: Float -> [Platform] -> Kario -> Enemy -> Enemy
stepEnemy secs platforms kario enemy = let prevPos = getPos enemy in moveAndCollide secs platforms . applyGravity secs . handleVelocity $ enemy

--by moving over the x-axis and y-axis seperately we avoid some bugs that arose from our collision implementation 
moveAndCollide :: Float -> [Platform] -> Enemy -> Enemy
moveAndCollide secs platforms enemy = handlePlatformCollisions (getPos enemy') platforms . moveY secs $ enemy'
    where
        enemy' = handlePlatformCollisions (getPos enemy) platforms . moveX secs $ enemy 

handlePlatformCollisions :: Position -> [Platform] -> Enemy -> Enemy
handlePlatformCollisions prevPos platforms movedEnemy = foldr (handleCollision prevPos) movedEnemy $ getOverlaps movedEnemy platforms

handleCollision :: Position -> Overlap -> Enemy -> Enemy
handleCollision (prevX,prevY) Hitbox{width = overlapX, height = overlapY} enemy
    | overlapX < overlapY = updateVel (- vx, vy) $  updatePos enemy (prevX, newY) --when the horizontal overlap is smaller we treat the collision as a horizontal one
    | otherwise           = updateVel (vx,0) $ updatePos enemy (newX, prevY)      --otherwise we treat it as a vertical collision where the enemy falls
    where (newX,newY) = getPos enemy
          (vx,vy)     = getVel enemy


handleCollisions :: [Platform] -> Enemy -> Enemy
handleCollisions = undefined

handleVelocity :: Enemy -> Enemy
handleVelocity enemy = case getVel enemy of
                         (0,0) -> setDefaultVelocity enemy
                         _     -> enemy

setDefaultVelocity :: Enemy -> Enemy
setDefaultVelocity k@Koomba{}      = k{enemyVel = (-50, 0)}
setDefaultVelocity k@KoopaTroopa{} = k{enemyVel = (-50,0)}
setDefaultVelocity k@KoopaShell{}  = k{enemyVel = (0,0)}