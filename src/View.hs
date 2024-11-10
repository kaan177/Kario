{-# language NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import GHC.Float (int2Float)
import MenuDrawer
import Data.Fixed
import Positioning
import Movable

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameMenu menu@MenuState {} s _ ) = drawMenu menu s 
viewPure (GameLevel LevelState {kario, platforms, coins, elapsedGameTime, enemies, flagPole, coinLevelScore, camera} sprites@Sprites{karioImage, coinPictures} _ ) = 
    Pictures [

    translate (-camX) (-camY) $ Pictures [
    drawKario kario karioImage,
    drawPlatforms platforms sprites,
    animateCoins coins coinPictures elapsedGameTime,
    Pictures $ map (drawEnemy sprites) enemies,     --draw all enemies
    drawFlagPole sprites flagPole ],
 
    drawCoinCounter coinLevelScore ]                --draw UIelement separately so it does not move along with camera
    where
        (camX, camY) = getPos camera

data Square = Sqr Point Point Point Point

------------------------------In game stuf----------------------------------------------------------------------------------
sqrToList :: Square -> [Point]
sqrToList (Sqr bl tl tr br) = [bl, tl, tr, br]

sqrFromSize :: Float -> Square
sqrFromSize s = Sqr (0,0) (0,s) (s,s) (s,0)

drawSquares :: Int -> Picture
drawSquares 0 = polygon (sqrToList (sqrFromSize 10))
drawSquares n = Pictures [polygon (sqrToList (sqrFromSize 10)), translate 20 0 (drawSquares (n - 1))]

drawKario :: Kario -> Picture -> Picture
drawKario kario = let (x,y) = getPos kario in translate x y . scale sFac 1
  where (vx, _) = getVel kario
        sFac    | vx < 0    =  -1 --mirror image
                | otherwise =   1 --don't mirror

drawPlatforms :: [Platform] -> Sprites -> Picture
drawPlatforms list sprites = Pictures (map (drawPlatform sprites) list)

drawPlatform :: Sprites -> Platform -> Picture
drawPlatform s (Ground (Hitbox (x,y) _ _)) = Translate x y (groundImage s)
drawPlatform s (Brick (Hitbox (x,y) _ _)) = Translate x y (brickImage s)
drawPlatform s (ItemBox (Hitbox (x,y) _ _)_) = Translate x y (questionMarkImage s)
drawPlatform s (EmptyItemBox (Hitbox (x,y) _ _)) = Translate x y (brokenQuestionMarkImage s)

animateCoins :: [Coin] -> [Picture] -> Float -> Picture
animateCoins list p time = Pictures (map (animateCoin p time) list)

animateCoin :: [Picture] -> Float -> Coin -> Picture
animateCoin p time (Coin (Hitbox (x,y) _ _) Bling _) | mod' time 5 <= 4 = translate x y (head p)
                                                     | otherwise = translate x y (head $ tail p)

drawEnemy :: Sprites -> Enemy -> Picture
drawEnemy s k@Koomba{} =      let (x,y) = getPos k in Translate x y (koombaImage s) 
drawEnemy s k@KoopaTroopa{} = let (x,y) = getPos k in Translate x y . scale sFac 1 $ koopaImage s 
  where (vx, _) = getVel k
        sFac    | vx < 0    =  1 --don't mirror
                | otherwise = -1 --mirror image
drawEnemy s k@KoopaShell{} = let (x,y) = getPos k in Translate x y (shellImage s) 

drawFlagPole :: Sprites -> FlagPole -> Picture
drawFlagPole s f@FlagPole{} = let (x,y) = getPos f in Translate x y (flagPoleImage s)

