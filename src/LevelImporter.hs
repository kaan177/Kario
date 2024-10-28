{-# language NamedFieldPuns #-}

module LevelImporter (levelBuilder) where


import Model
import GHC.Float (floorFloat)


levelBuilder :: String -> LevelState
levelBuilder s = recursiveLevelBuilder (listMaker s) emptyLevel

recursiveLevelBuilder :: [(Char, Float, Float)] -> LevelState -> LevelState
recursiveLevelBuilder [] l = l
recursiveLevelBuilder (('K', x, y) : as) levelState@(LevelState {kario}) = recursiveLevelBuilder as levelState {kario = Kario (Hitbox (x * gridSize, y * gridSize) 20 20) (10, 0) (0,0) Grounded}
recursiveLevelBuilder (('C', x, y) : as) levelState@(LevelState {coins}) = recursiveLevelBuilder as levelState {coins = Coin (Hitbox (x * gridSize, y * gridSize) 30 30) Bling Exist : coins}
recursiveLevelBuilder (('G', x, y) : as) levelState@(LevelState {platforms}) = recursiveLevelBuilder as levelState { platforms = Ground (Hitbox (x * gridSize, y * gridSize) 30 30) : platforms}
recursiveLevelBuilder (('B', x, y) : as) levelState@(LevelState {platforms}) = recursiveLevelBuilder as levelState { platforms = Brick (Hitbox (x * gridSize, y * gridSize) 30 30) : platforms}
recursiveLevelBuilder (('M', x, y) : as) levelState@(LevelState {platforms}) = recursiveLevelBuilder as levelState { platforms = ItemBox (Hitbox (x * gridSize, y * gridSize) 30 30) Mushroom : platforms}
recursiveLevelBuilder (('S', x, y) : as) levelState@(LevelState {platforms}) = recursiveLevelBuilder as levelState { platforms = ItemBox (Hitbox (x * gridSize, y * gridSize) 30 30) Star : platforms}
recursiveLevelBuilder (('O', x, y) : as) levelState = recursiveLevelBuilder as levelState
recursiveLevelBuilder ((_, x, y) : as) levelState = error "Character is in level loading is invalid."
{-
K = Kario
C = Coin
G = Ground
B = brick
M = mushroom itembox
S = star itembox
R = random itembox (nog niet geimplementeerd)
O = air (grote o niet nul)
-}

emptyLevel :: LevelState
emptyLevel = LevelState{
  kario = Kario (Hitbox (0,0) 20 20) (10, 0) (0,0) Grounded,
  lilInt = 1,
  platforms = [],
  coins = [],
  elapsedGameTime = 0
}

listMaker :: String -> [(Char, Float, Float)]
listMaker str = recursiveListMaker str (fst adjustedScreensize) (snd adjustedScreensize)

recursiveListMaker :: String -> Float -> Float -> [(Char, Float, Float)]
recursiveListMaker [] _ _ = []
recursiveListMaker ('\n' : xs) _ y = recursiveListMaker xs (fst adjustedScreensize) (y - 1)
recursiveListMaker (a : as) x y = (a, x, y) : recursiveListMaker as (x + 1) y

adjustedScreensize :: (Float, Float)
adjustedScreensize = (((((fromIntegral(fst screenSize))/(gridSize * (-2))))), (((fromIntegral(snd screenSize))/(gridSize * (2)))))