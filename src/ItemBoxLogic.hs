{-# language NamedFieldPuns #-}

module ItemBoxLogic where

import Model
import Collision (isColliding, getOverlap, Collidable (getBox))
import Data.Char (GeneralCategory(NotAssigned))
import Movable (Movable(getVel))
import LevelImporter (gridSize)
stepItemBox :: Kario -> Float -> Platform -> (Platform, Maybe PowerUp)
stepItemBox k rand (ItemBox _ _) = undefined
stepItemBox _ rand p = (p, Nothing)

checkKarioCollideItemBox :: Kario -> Float -> Platform -> (Platform, Maybe PowerUp)
checkKarioCollideItemBox kario rand p | isColliding kario p = case getOverlap (getBox kario) (getBox p) of
    Nothing      -> (p, Nothing)
    Just overlap -> handleKarioCollideItemBox kario overlap rand p

handleKarioCollideItemBox :: Kario -> Overlap -> Float -> Platform -> (Platform, Maybe PowerUp)
handleKarioCollideItemBox kario Hitbox{width = overX, height = overY} rand platform
  | overX < overY = (platform, Nothing)                          --enemy does not change, kario dies
  | karVelY > 0   = karioCollideItemBox kario rand platform                      --kario jumps on enemy, enemy dies
  | otherwise     = (platform, Nothing)                        --enemy does not change, kario dies
    where karVelY = snd $ getVel kario

karioCollideItemBox :: Kario -> Float -> Platform -> (Platform, Maybe PowerUp)
karioCollideItemBox kario rand (ItemBox h (Just m@Mushroom{})) = (EmptyItemBox h, Just (m{hitbox = (\newh@Hitbox{pos = (x,y)} -> newh{pos = (x+gridSize, y + gridSize)}) h}))
karioCollideItemBox kario rand (ItemBox h (Just s@Star{})) = (EmptyItemBox h, Just (s{hitbox = (\newh@Hitbox{pos = (x,y)} -> newh{pos = (x+gridSize, y + gridSize)}) h}))
karioCollideItemBox kario rand (ItemBox h Nothing)| rand <= starChance = (EmptyItemBox h, Just (Star{hitbox = (\newh@Hitbox{pos = (x,y)} -> newh{pos = (x+gridSize, y + gridSize)}) h, powerUpvel = (0,0), powerShouldExist = Exist}))
                                                  | otherwise = (EmptyItemBox h, Just (Mushroom{hitbox = (\newh@Hitbox{pos = (x,y)} -> newh{pos = (x+gridSize, y + gridSize)}) h, powerUpvel = (0,0), powerShouldExist = Exist}))


starChance :: Float
starChance = 0.1