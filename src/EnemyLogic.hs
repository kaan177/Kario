{-# language NamedFieldPuns #-}

module EnemyLogic(stepEnemy) where

import Model
import Accelerable ( Accelerable(updateVel, applyGravity) )
import Collision
import Movable
import Accelerable (Accelerable(applyGravity))
import Data.IntMap (update)
import Animation

--should add logic for when out of screen not move
stepEnemy :: Float -> [Platform] -> Kario -> Enemy -> Enemy
stepEnemy secs platforms kario enemy = let prevPos = getPos enemy in 
      moveAndCollide secs platforms
    . applyGravity secs 
    . handleVelocity 
    . checkKarioCollison kario 
    . updateAnimation secs $ enemy

checkKarioCollison :: Kario -> Enemy -> Enemy
checkKarioCollison kario enemy = case getOverlap (getBox kario) (getBox enemy) of
    Nothing      -> enemy
    Just overlap -> handleKarioCollision overlap kario enemy 

handleKarioCollision :: Overlap -> Kario -> Enemy -> Enemy
handleKarioCollision Hitbox{width = overX, height = overY} kario enemy
  | overX < overY = enemy                           --enemy does not change, kario dies
  | karVelY < 0   = die enemy                       --kario jumps on enemy, enemy dies
  | otherwise     = enemy                           --enemy does not change, kario dies
    where karVelY = snd $ getVel kario
          die e@Koomba{}       = e{enemyExist = RemoveIn 0}
          die e@KoopaTroopa {} = KoopaShell (getBox e) (0,0) Exist (EnemyMoving 0 frameDuration) --koopa should turn into shell
          die e@KoopaShell {}  = e{enemyExist = RemoveIn 0}


--by moving over the x-axis and y-axis seperately we avoid some bugs that arose from our collision implementation 
moveAndCollide :: Float -> [Platform] -> Enemy -> Enemy
moveAndCollide secs platforms enemy = foldl collisionFailSafe (handlePlatformCollisions (getPos enemy') platforms . moveY secs $ enemy') platforms
    where
        enemy' = handlePlatformCollisions (getPos enemy) platforms . moveX secs $ enemy

handlePlatformCollisions :: Position -> [Platform] -> Enemy -> Enemy
handlePlatformCollisions prevPos platforms movedEnemy = foldr (handleCollision prevPos) movedEnemy $ getOverlaps movedEnemy platforms

handleCollision :: Position -> Overlap -> Enemy -> Enemy
handleCollision (prevX,prevY) Hitbox{width = overlapX, height = overlapY} enemy
    | overlapX < (overlapY - overlapYBias) = updateVel (- vx, vy) $ updatePos enemy (prevX, newY) --when the horizontal overlap is smaller we treat the collision as a horizontal one
    | otherwise                            = updateVel (vx,0) $ updatePos enemy (newX, prevY)      --otherwise we treat it as a vertical collision where the enemy falls
    where (newX,newY) = getPos enemy
          (vx,vy)     = getVel enemy

handleVelocity :: Enemy -> Enemy
handleVelocity enemy = case getVel enemy of
                         (0,0) -> setDefaultVelocity enemy
                         _     -> enemy

setDefaultVelocity :: Enemy -> Enemy
setDefaultVelocity k@Koomba{}      = k{enemyVel = (-50, 0)}
setDefaultVelocity k@KoopaTroopa{} = k{enemyVel = (-50,0)}
setDefaultVelocity k@KoopaShell{}  = k{enemyVel = (100,100)}