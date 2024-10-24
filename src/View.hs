{-# language NamedFieldPuns #-}
-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import GHC.Float (int2Float)
import Data.Fixed

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameMenu(MenuState s) _) = translate (-200) 0.0 (color red (text s))
viewPure (GameLevel LevelState {kario, lilInt, platforms, coins, elapsedGameTime} sprites@Sprites{karioImage, coinPictures}) = Pictures [
    drawKario kario karioImage,
    drawPlatforms platforms sprites,
    color blue (drawSquares lilInt),
    animateCoins coins coinPictures elapsedGameTime
    ]

data Square = Sqr Point Point Point Point


sqrToList :: Square -> [Point]
sqrToList (Sqr bl tl tr br) = [bl, tl, tr, br]

sqrFromSize :: Float -> Square
sqrFromSize s = Sqr (0,0) (0,s) (s,s) (s,0)

drawSquares :: Int -> Picture
drawSquares 0 = polygon (sqrToList (sqrFromSize 10))
drawSquares n = Pictures [polygon (sqrToList (sqrFromSize 10)), translate 20 0 (drawSquares (n - 1))]

drawKario :: Kario -> Picture -> Picture
drawKario Kario{hitbox = Hitbox {pos = (x,y)}} = translate x y

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