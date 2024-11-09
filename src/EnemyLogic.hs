{-# language NamedFieldPuns #-}

module EnemyLogic(stepEnemy) where

import Model
import Collision
import Movable
import Accelerable (Accelerable(applyGravity))

stepEnemy :: Float -> [Platform] -> Kario -> Enemy -> Enemy
stepEnemy secs platforms kario = move secs . applyGravity secs . handleVelocity

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