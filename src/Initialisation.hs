module Initialisation where

import Model
import UI

initialState :: Sprites -> [String] -> CoinScore -> GameState
initialState s l c = GameMenu (initialMenuState l c) s l 

initialMenuState :: [String] -> CoinScore -> MenuState
initialMenuState l c =
    let selectedLevelInt = if Prelude.null l then Nothing
            else Just 0 in
    let selectedLevelObject = if Prelude.null l then Nothing
            else Just generateSelector in
    MenuState{
    selectedLevel = selectedLevelInt,
    gameScreen = GameScreen $ Hitbox ((\(x,y) -> (0, 0)) screenSize) 0 0,
    levelButtons = generateLevelButtons l,
    selector = selectedLevelObject,
    coinMenuScore = c
}