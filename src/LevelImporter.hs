{-# language NamedFieldPuns #-}

module LevelImporter (levelBuilder) where

import Model
import Positioning
import GHC.Float (int2Float)

levelBuilder :: LevelContents -> CoinScore -> LevelState
levelBuilder s c = recursiveLevelBuilder (listMaker s) (emptyLevel c)

recursiveLevelBuilder :: [(Char, Float, Float)] -> LevelState -> LevelState
recursiveLevelBuilder [] l = l
recursiveLevelBuilder (('K', x, y) : as) levelState@(LevelState {kario, camera}) = let karioPos = (x * gridSize + (30 - gridSize), y * gridSize + (45 - gridSize)) in recursiveLevelBuilder as levelState {
  kario  = Kario (Hitbox karioPos 30 45) 0 (0, 0) (0,0) Grounded Exist,
  camera = updatePos camera karioPos }
recursiveLevelBuilder (('k', x, y) : as) levelState@(LevelState {enemies})       = recursiveLevelBuilder as levelState {enemies = KoopaTroopa (Hitbox (x * gridSize + (30 - gridSize), y * gridSize + (38 - gridSize)) 30 38) (0,0) Exist : enemies }
recursiveLevelBuilder (('C', x, y) : as) levelState@(LevelState {coins})         = recursiveLevelBuilder as levelState {coins = Coin (Hitbox (x * gridSize, y * gridSize) 30 30) Bling Exist : coins}
recursiveLevelBuilder (('G', x, y) : as) levelState@(LevelState {platforms})     = recursiveLevelBuilder as levelState { platforms = Ground (Hitbox (x * gridSize, y * gridSize) 30 30) : platforms}
recursiveLevelBuilder (('B', x, y) : as) levelState@(LevelState {platforms})     = recursiveLevelBuilder as levelState { platforms = Brick (Hitbox (x * gridSize, y * gridSize) 30 30) : platforms}
recursiveLevelBuilder (('M', x, y) : as) levelState@(LevelState {platforms})     = recursiveLevelBuilder as levelState { platforms = ItemBox (Hitbox (x * gridSize, y * gridSize) 30 30) Mushroom : platforms}
recursiveLevelBuilder (('S', x, y) : as) levelState@(LevelState {platforms})     = recursiveLevelBuilder as levelState { platforms = ItemBox (Hitbox (x * gridSize, y * gridSize) 30 30) Star : platforms}
recursiveLevelBuilder (('g', x, y) : as) levelState@(LevelState {enemies})       = recursiveLevelBuilder as levelState {enemies = Koomba (Hitbox (x * gridSize + (30 - gridSize), y * gridSize + (30 - gridSize)) 30 30) (0,0) Exist : enemies }
recursiveLevelBuilder (('F', x, y) : as) levelState@(LevelState {flagPole})      = recursiveLevelBuilder as levelState {flagPole = FlagPole (Hitbox (x * gridSize , y * gridSize + 135 ) 10 300)  }
recursiveLevelBuilder (('O', x, y) : as) levelState                              = recursiveLevelBuilder as levelState
recursiveLevelBuilder ((_, x, y) : as) levelState = error "Character in level loading is invalid."
{-
K = Kario
k = KoopaTroopa
C = Coin
G = Ground
g = Koomba
B = brick
M = mushroom itembox
S = star itembox
R = random itembox (nog niet geimplementeerd)
O = air (grote o niet nul)
F = FlagPole
-}

emptyLevel :: CoinScore -> LevelState
emptyLevel coinScore = LevelState{
  kario           = Kario (Hitbox (0,0) 20 20) 0 (10, 0) (0,0) Grounded Exist,
  platforms       = [],
  coins           = [],
  elapsedGameTime = 0,
  inputState      = [],
  enemies         = [],
  flagPole        = FlagPole (Hitbox (150,135) 30 300),
  coinLevelScore  = coinScore,
  camera          = Camera (Hitbox (0,0) (int2Float scrX) (int2Float scrY)) (0,0) }
  where
    (scrX, scrY) = screenSize

listMaker :: String -> [(Char, Float, Float)]
listMaker str = recursiveListMaker str (fst adjustedScreensize) (snd adjustedScreensize)

recursiveListMaker :: String -> Float -> Float -> [(Char, Float, Float)]
recursiveListMaker [] _ _ = []
recursiveListMaker ('\n' : xs) _ y = recursiveListMaker xs (fst adjustedScreensize) (y - 1)
recursiveListMaker (a : as) x y = (a, x, y) : recursiveListMaker as (x + 1) y

adjustedScreensize :: (Float, Float)
adjustedScreensize = (fromIntegral (fst screenSize) / (gridSize * (-2)), fromIntegral (snd screenSize) / (gridSize * 2))