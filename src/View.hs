{-# language NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import GHC.Float (int2Float)
import UI
import Data.Fixed
import Movable
import Collision

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameMenu menu@MenuState {} s _ ) = drawMenu menu s
viewPure (GameLevel LevelState {kario, platforms, coins, elapsedGameTime, enemies, flagPole, coinLevelScore, camera, powerups} sprites@Sprites{coinPictures} _ ) =
    Pictures [

    translate (-camX) (-camY) $ Pictures [
    drawKario kario elapsedGameTime (getKarioPic sprites kario) ,
    drawPlatforms platforms sprites,
    Pictures $ map (drawCoin coinPictures) coins,
    Pictures $ map (drawEnemy sprites) enemies,     --draw all enemies
    drawFlagPole sprites flagPole,
    Pictures (map (drawPowerUps sprites) powerups) ],

    drawCoinCounter coinLevelScore ]                --draw UIelement separately so it does not move along with camera
    where
        (camX, camY) = getPos camera

data Square = Sqr Point Point Point Point

------------------------------In game stuf----------------------------------------------------------------------------------
drawCoin :: [Picture] -> Coin -> Picture
drawCoin frames (Coin Hitbox{pos} (Bling nr _) _) = uncurry Translate pos (frames !! (nr `div` 5))

drawKario :: Kario -> Float -> Picture -> Picture
drawKario kario secs = let (x,y) = getPos kario in translate x y . starCheck kario secs . scale sFac (bigFactor kario)
  where (vx, _) = getVel kario
        sFac    | vx < 0    =  -1 --mirror image
                | otherwise =   1 --don't mirror
        bigFactor Kario{powerUp = Big}= 1.5
        bigFactor Kario{powerUp = Small}= 1
        starCheck Kario{invincibleState = Invincible _} secs = Rotate (secs * rotationSpeed)
        starCheck _ _ = id

rotationSpeed :: Float
rotationSpeed = 500

getKarioPic :: Sprites -> Kario -> Picture
getKarioPic s Kario{karAnim = Idle}    = karioImage s
getKarioPic s Kario{karAnim = Jumping} = karioJumpingImage s
getKarioPic s Kario{karAnim = Walking 3 _} = karioWalkingImages s !! 1
getKarioPic s Kario{karAnim = Walking n _} = karioWalkingImages s !! n
getKarioPic s Kario{karAnim = Dying _ _} = undefined

drawPlatforms :: [Platform] -> Sprites -> Picture
drawPlatforms list sprites = Pictures (map (drawPlatform sprites) list)

drawPlatform :: Sprites -> Platform -> Picture
drawPlatform s (Ground (Hitbox (x,y) _ _)) = Translate x y (groundImage s)
drawPlatform s (Brick (Hitbox (x,y) _ _)) = Translate x y (brickImage s)
drawPlatform s (ItemBox (Hitbox (x,y) _ _)_) = Translate x y (questionMarkImage s)
drawPlatform s (EmptyItemBox (Hitbox (x,y) _ _)) = Translate x y (brokenQuestionMarkImage s)

drawEnemy :: Sprites -> Enemy -> Picture
drawEnemy s k@KoopaShell{} = let (x,y) = getPos k in Translate x y (shellImage s)
drawEnemy s k@Koomba{enemyAnim = EnemyMoving 3 _} = let (x,y)  = getPos k in Translate x y $ koombaMovingImages s !! 1
drawEnemy s k@Koomba{enemyAnim = EnemyMoving nr _} = let (x,y) = getPos k in Translate x y $ koombaMovingImages s !! nr
drawEnemy s k@KoopaTroopa{enemyAnim = EnemyMoving nr _} = let (x,y) = getPos k in Translate x y . scale sFac 1 $ pic nr
  where (vx, _) = getVel k
        pic 3       = koopaMovingImages s !! 1
        pic frameNr = koopaMovingImages s !! frameNr
        sFac    | vx < 0    =  1 --don't mirror
                | otherwise = -1 --mirror image

drawFlagPole :: Sprites -> FlagPole -> Picture
drawFlagPole s f@FlagPole{} = let (x,y) = getPos f in Translate x y (flagPoleImage s)

drawPowerUps :: Sprites -> PowerUp -> Picture
drawPowerUps s p@Mushroom{} = let (x,y) = getPos p in Translate x y (mushroomImage s)
drawPowerUps s p@Star{} = let (x,y) = getPos p in Translate x y (starImage s)