{-# language NamedFieldPuns #-}

module UI (drawMenu, drawCoinCounter, generateLevelButtons, generateSelector, buttonPosition) where

import Model
import Graphics.Gloss
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Interface.Environment (getScreenSize)
import GHC.Float (int2Float)

import Data.Sequence
import Data.Foldable

drawMenu :: MenuState -> Sprites -> Picture
drawMenu MenuState{gameScreen, levelButtons, selector, coinMenuScore} Sprites{menuImage, levelBoxImage, selectionRingImage} =
  Pictures ([
    uncurry Translate ((\(GameScreen(Hitbox (x,y) _ _))-> (x,y)) gameScreen) menuImage,
    drawSelector selector selectionRingImage,
    drawCoinCounter coinMenuScore]
    ++ map (drawButton levelBoxImage) levelButtons)

drawSelector :: Maybe Selector -> Picture -> Picture
drawSelector (Just (Selector (Hitbox (x,y) _ _))) p = Translate x y p
drawSelector Nothing p = Blank

drawButton ::  Picture -> LevelButton -> Picture
drawButton p (LevelButton (Hitbox (x, y) _ _) i) = Pictures [Translate x y p, Translate (x - (buttonSize/2) + 10) (y- (buttonSize/2) + 4) (Scale 0.4 0.4 (Text (show i))) ]

drawCoinCounter :: Int -> Picture
drawCoinCounter c = uncurry Translate ((\(x,y) -> (int2Float x * 0.40, int2Float y * 0.40)) screenSize) (Scale 0.4 0.4 (color yellow $ Text (show c)))



buttonPosition :: Int -> (Float, Float)
buttonPosition i = (\(x,y) i -> (x + ((buttonSize + buttonPadding) * int2Float(mod (floor i) (floor buttonAmountX))), y - ((buttonSize + buttonPadding) * int2Float(mod (floor (i/buttonAmountX)) (floor buttonAmountY))))) buttonStartPosition (int2Float i)

----------------------------------------------------------------------------------------
--Generation

generateLevelButtons :: [String] -> [LevelButton]
generateLevelButtons l = toList (mapWithIndex (\i _-> LevelButton (Hitbox (buttonPosition i) buttonSize buttonSize) i ) (fromList l))

generateSelector :: Selector
generateSelector = Selector (Hitbox buttonStartPosition 0 0)

----------------------------------------------------------------------------------------
--ButtonConstants
buttonSize :: Float
buttonSize = 60

buttonPadding :: Float
buttonPadding = 30

buttonStartPosition :: (Float,Float)
buttonStartPosition = (\(x,y) -> ((int2Float x)/4 - (int2Float x)/2,(int2Float y)/2 - (int2Float y)/2)) screenSize

buttonAmountX :: Float
buttonAmountX = 4

buttonAmountY :: Float
buttonAmountY = 2