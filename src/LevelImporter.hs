{-# language NamedFieldPuns #-}

module LevelImporter (levelBuilder) where

import Model
import GHC.Float (floorFloat)
import GHC.IO.Encoding.CodePage (codePageEncoding)

levelBuilder :: LevelContents -> CoinScore -> LevelState
levelBuilder s c = recursiveLevelBuilder (listMaker s) (emptyLevel c)

recursiveLevelBuilder :: [(Char, Float, Float)] -> LevelState -> LevelState
recursiveLevelBuilder [] l = l
recursiveLevelBuilder (('K', x, y) : as) levelState@(LevelState {kario}) = recursiveLevelBuilder as levelState {kario = Kario (Hitbox (x * gridSize + (30 - gridSize), y * gridSize + (45 - gridSize)) 30 45) 0 (0, 0) (0,0) Grounded Small}
recursiveLevelBuilder (('C', x, y) : as) levelState@(LevelState {coins}) = recursiveLevelBuilder as levelState {coins = Coin (Hitbox (x * gridSize, y * gridSize) 30 30) Bling Exist : coins}
recursiveLevelBuilder (('G', x, y) : as) levelState@(LevelState {platforms}) = recursiveLevelBuilder as levelState { platforms = Ground (Hitbox (x * gridSize, y * gridSize) 30 30) : platforms}
recursiveLevelBuilder (('B', x, y) : as) levelState@(LevelState {platforms}) = recursiveLevelBuilder as levelState { platforms = Brick (Hitbox (x * gridSize, y * gridSize) 30 30) : platforms}
recursiveLevelBuilder (('M', x, y) : as) levelState@(LevelState {platforms}) = recursiveLevelBuilder as levelState { platforms = ItemBox (Hitbox (x * gridSize, y * gridSize) 30 30) (Mushroom (Hitbox(0,0) 0 0)): platforms}
recursiveLevelBuilder (('S', x, y) : as) levelState@(LevelState {platforms}) = recursiveLevelBuilder as levelState { platforms = ItemBox (Hitbox (x * gridSize, y * gridSize) 30 30) (Star (Hitbox(0,0) 0 0)): platforms}
recursiveLevelBuilder (('g', x, y) : as) levelState@(LevelState {enemies}) = recursiveLevelBuilder as levelState {enemies = Koomba (Hitbox (x * gridSize + (30 - gridSize), y * gridSize + (30 - gridSize)) 30 30) (0,0) Exist : enemies }
recursiveLevelBuilder (('F', x, y) : as) levelState@(LevelState {flagPole}) = recursiveLevelBuilder as levelState {flagPole = FlagPole (Hitbox (x * gridSize , y * gridSize + 135 ) 10 300)  }
recursiveLevelBuilder (('m', x, y) : as) levelState@(LevelState {powerups}) = recursiveLevelBuilder as levelState {powerups = Mushroom (Hitbox (x * gridSize , y * gridSize) 20 20) : powerups  }
recursiveLevelBuilder (('s', x, y) : as) levelState@(LevelState {powerups}) = recursiveLevelBuilder as levelState {powerups = Star (Hitbox (x * gridSize , y * gridSize ) 20 20) : powerups }
recursiveLevelBuilder ((_, x, y) : as) levelState = recursiveLevelBuilder as levelState
--recursiveLevelBuilder ((_, x, y) : as) levelState = error "Character in level loading is invalid."
{-
K = Kario
C = Coin
G = Ground
B = brick
M = mushroom itembox
S = star itembox
R = random itembox (nog niet geimplementeerd)
O = air (grote o niet nul)
F = FlagPole
m = mushroom
s = star
-}

emptyLevel :: CoinScore -> LevelState
emptyLevel c = LevelState{
  kario = Kario (Hitbox (0,0) 20 20) 0 (10, 0) (0,0) Grounded Small,
  platforms = [],
  coins = [],
  elapsedGameTime = 0,
  inputState = [], 
  enemies = [],
  powerups = [],
  flagPole = FlagPole(Hitbox (150,135)30 300),
  coinLevelScore = c
}

listMaker :: String -> [(Char, Float, Float)]
listMaker str = recursiveListMaker str (fst adjustedScreensize) (snd adjustedScreensize)

recursiveListMaker :: String -> Float -> Float -> [(Char, Float, Float)]
recursiveListMaker [] _ _ = []
recursiveListMaker ('\n' : xs) _ y = recursiveListMaker xs (fst adjustedScreensize) (y - 1)
recursiveListMaker (a : as) x y = (a, x, y) : recursiveListMaker as (x + 1) y

adjustedScreensize :: (Float, Float)
adjustedScreensize = (((((fromIntegral(fst screenSize))/(gridSize * (-2))))), (((fromIntegral(snd screenSize))/(gridSize * (2)))))