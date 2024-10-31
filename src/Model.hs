{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

---------------------------------------------------------------
-- | Collidable type class
class Collidable a where
    getBox :: a -> Hitbox

instance Collidable Kario where
    getBox Kario{hitbox} = hitbox
instance Collidable Platform where
    getBox (Ground hitbox)       = hitbox
    getBox (Brick hitbox)        = hitbox
    getBox (BreakBrick hitbox _) = hitbox
    getBox (ItemBox hitbox _)    = hitbox
    getBox (EmptyItemBox hitbox) = hitbox


----------------------------------------------------------------
-- | Model

data Sprites = Sprites{
    karioImage :: Picture, 
    groundImage :: Picture,
    brickImage :: Picture,
    questionMarkImage :: Picture,
    brokenQuestionMarkImage :: Picture,
    coinPictures :: [Picture]
}

data GameState = GameLevel LevelState Sprites [(String)] | GameMenu MenuState Sprites [(String)]

data LevelState = LevelState {
    kario :: Kario,
    platforms :: [Platform],
    coins :: [Coin],
    elapsedGameTime :: Float
    }

data MenuState = MenuState GameName

type Position = Point
type Width = Float
type Height = Float
type DirectionalVelocity = Vector
type DirectionalAcceleration = Vector
data ShouldExist = Exist | RemoveIn Int

data Kario = Kario {
    hitbox :: Hitbox
    ,desiredHorizontalVelocity :: Float
    ,dirVelocity :: DirectionalVelocity
    ,dirAccel :: DirectionalAcceleration
    ,airborne :: Airborne
}
data Platform = Ground Hitbox | Brick Hitbox | BreakBrick Hitbox ShouldExist | ItemBox Hitbox PowerUpType | EmptyItemBox Hitbox

data PowerUpType = Mushroom | Star

data Coin = Coin Hitbox CoinAnimation ShouldExist

data Hitbox = Hitbox {
    pos :: Position,
    width :: Width,
    height :: Height
    }  --origin in centre

data Airborne = Grounded | Falling | Rising

data CoinAnimation = Bling | Collecting Float

gridSize :: Float
gridSize = 30
type GameName = String

initialState :: Sprites -> [String] -> GameState
initialState = GameMenu (MenuState "Kario") 

initialLevelState :: LevelState
initialLevelState = LevelState {
  kario = Kario (Hitbox (-40,50) 30 45) 0 (0, 0) (0,0) Falling,
  platforms = [Ground (Hitbox (0,(-1) * gridSize) 30 30), Ground (Hitbox ((-1) * gridSize,(-1) * gridSize) 30 30), Ground (Hitbox ((-2) * gridSize,(-1) * gridSize) 30 30), Brick (Hitbox (0, 4 * gridSize) 30 30), ItemBox (Hitbox (1 * gridSize, 4 * gridSize) 30 30) Mushroom, EmptyItemBox (Hitbox (2 * gridSize, 4 * gridSize) 30 30)] ,
  coins = [Coin (Hitbox (0 * gridSize, 0 * gridSize) 30 30) Bling Exist , Coin (Hitbox (6 * gridSize, 4 * gridSize) 30 30) Bling Exist , Coin (Hitbox (7 * gridSize, 4 * gridSize) 30 30) Bling Exist],
  elapsedGameTime = 0
  }


screenSize :: (Int,Int)
screenSize = (600,600)
