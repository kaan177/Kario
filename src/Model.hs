{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss
import GHC.Float (int2Float)

---------------------------------------------------------------
-- | General constants

screenSize :: (Int,Int)
screenSize = (600,600)

----------------------------------------------------------------
-- | Model

data Sprites = Sprites{
    karioImage :: Picture,
    karioWalkingImages :: [Picture],
    karioJumpingImage :: Picture,
    groundImage :: Picture,
    brickImage :: Picture,
    questionMarkImage :: Picture,
    brokenQuestionMarkImage :: Picture,
    coinPictures :: [Picture],
    menuImage :: Picture,
    levelBoxImage :: Picture,
    selectionRingImage :: Picture,
    koombaMovingImages :: [Picture],
    koopaMovingImages :: [Picture],
    shellImage :: Picture,
    flagPoleImage :: Picture,
    starImage :: Picture,
    mushroomImage :: Picture
}

type LevelContents = String

data GameState = GameLevel LevelState Sprites [LevelContents] | GameMenu MenuState Sprites [LevelContents]

type Inputs = [Char] --all keys that are currently down.

type CoinScore = Int

data LevelState = LevelState {
    kario           :: Kario,
    platforms       :: [Platform],
    coins           :: [Coin],
    elapsedGameTime :: Float,
    inputState      :: Inputs,
    enemies         :: [Enemy],
    powerups        :: [PowerUp],
    flagPole        :: FlagPole,
    coinLevelScore  :: CoinScore,
    camera          :: Camera
    }

data MenuState = MenuState {
    selectedLevel :: Maybe Int,
    gameScreen :: GameScreen,
    levelButtons :: [LevelButton],
    selector :: Maybe Selector,
    coinMenuScore :: CoinScore
}

type Position = Point
type Width = Float
type Height = Float
type DirectionalVelocity = Vector
type DirectionalAcceleration = Vector
data ShouldExist = Exist 
                 | RemoveIn Float

data Kario = Kario {
    karHitbox   :: Hitbox
    ,desiredHorizontalVelocity :: Float
    ,powerUp    :: PowerUpType
    ,karVel     :: DirectionalVelocity
    ,karAccel   :: DirectionalAcceleration
    ,airborne   :: Airborne
    ,karioExist :: ShouldExist
    ,karAnim    :: KarioAnimation
}

type FrameNr = Int

data KarioAnimation = Idle 
                    | Walking FrameNr Float
                    | Jumping 
                    | Dying   FrameNr Float

data Camera = Camera Hitbox DirectionalVelocity

data Enemy = Koomba      { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }
           | KoopaTroopa { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }     
           | KoopaShell  { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }

data EnemyAnimation = EnemyMoving FrameNr Float

data Platform = Ground Hitbox | Brick Hitbox | BreakBrick Hitbox ShouldExist | ItemBox Hitbox PowerUp | EmptyItemBox Hitbox

data PowerUpType = Big | Invincible | Small
data PowerUp = Mushroom {hitbox :: Hitbox, powerUpvel :: DirectionalVelocity, powerShouldExist :: ShouldExist} | Star {hitbox :: Hitbox, powerUpvel :: DirectionalVelocity, powerShouldExist :: ShouldExist}

data Coin = Coin Hitbox CoinAnimation ShouldExist

data FlagPole = FlagPole Hitbox

data Hitbox = Hitbox {
    pos :: Position,
    width :: Width,
    height :: Height
    }  --origin in centre

type Overlap = Hitbox

data Airborne = Grounded | Airborne deriving Eq

data CoinAnimation = Bling | Collecting Float

type GameName = String

----------------------------------------------------------------
-- | UI
data GameScreen = GameScreen Hitbox
data LevelButton = LevelButton Hitbox Int
data Selector = Selector Hitbox
