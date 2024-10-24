{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data Sprites = Sprites{
    karioImage :: Picture, 
    groundImage :: Picture,
    brickImage :: Picture,
    questionMarkImage :: Picture,
    brokenQuestionMarkImage :: Picture,
    coinImage :: Picture
}


data GameState = GameLevel LevelState Sprites | GameMenu MenuState Sprites

data LevelState = LevelState {
    kario :: Kario,
    lilInt :: Int,
    platforms :: [Platform],
    coins :: [Coin]
    }

data MenuState = MenuState GameName

data Kario = Kario {
    hitbox :: Hitbox
    ,dirVelocity :: DirectionalVelocity
}
data Platform = Ground Hitbox | Brick Hitbox | BreakBrick Hitbox ShouldExist | ItemBox Hitbox PowerUpType | EmptyItemBox Hitbox

data PowerUpType = Mushroom | Star

data Coin = Coin Hitbox ShouldExist

data Hitbox = Hitbox {
    pos :: Position,
    width :: Width,
    height :: Height
    }  --origin in centre

type Position = Point
type Width = Float
type Height = Float
type DirectionalVelocity = Vector
data ShouldExist = Exist | RemoveIn Int

gridSize :: Float
gridSize = 30
type GameName = String

initialState :: Sprites -> GameState
initialState = GameMenu (MenuState "Kario")

initialLevelState :: LevelState
initialLevelState = LevelState 
  (Kario (Hitbox (0,0) 20 20) (10, 0)) 1 
  [Ground (Hitbox (0,(-1) * gridSize) 30 30), Brick (Hitbox (0, 4 * gridSize) 30 30), ItemBox (Hitbox (1 * gridSize, 4 * gridSize) 30 30) Mushroom, EmptyItemBox (Hitbox (2 * gridSize, 4 * gridSize) 30 30)]
  [Coin (Hitbox (5 * gridSize, 4 * gridSize) 30 30) Exist, Coin (Hitbox (6 * gridSize, 4 * gridSize) 30 30) Exist, Coin (Hitbox (7 * gridSize, 4 * gridSize) 30 30) Exist]