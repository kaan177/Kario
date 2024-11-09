{-# language NamedFieldPuns #-}

module MenuDrawer (drawMenu) where

import Model
import Graphics.Gloss
import Data.Maybe (fromMaybe)

drawMenu :: MenuState -> Sprites -> Picture
drawMenu MenuState{gameScreen, levelButtons, selector} Sprites{menuImage, levelBoxImage, selectionRingImage} =
  Pictures ([
    uncurry Translate ((\(GameScreen(Hitbox (x,y) _ _))-> (x,y)) gameScreen) menuImage,
    drawSelector selector selectionRingImage]
    ++ map (drawButton levelBoxImage) levelButtons)

drawSelector :: Maybe Selector -> Picture -> Picture
drawSelector (Just (Selector (Hitbox (x,y) _ _))) p = Translate x y p
drawSelector Nothing p = Blank

drawButton ::  Picture -> LevelButton -> Picture
drawButton p (LevelButton (Hitbox (x, y) _ _) i) = Pictures [Translate x y p, Translate (x - (buttonSize/2) + 10) (y- (buttonSize/2) + 4) (Scale 0.4 0.4 (Text (show i))) ]



