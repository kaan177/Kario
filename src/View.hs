-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import GHC.Float (int2Float)

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameMenu(MenuState s)) = translate (-200) 0.0 (color red (text s))
viewPure (GameLevel (LevelState n)) = color blue (drawSquares n)

data Square = Sqr Point Point Point Point

sqrToList :: Square -> [Point]
sqrToList (Sqr bl tl tr br) = [bl, tl, tr, br]

sqrFromSize :: Float -> Square
sqrFromSize s = Sqr (0,0) (0,s) (s,s) (s,0)

drawSquares :: Int -> Picture
drawSquares 0 = polygon (sqrToList (sqrFromSize 10))
drawSquares n = Pictures [polygon (sqrToList (sqrFromSize 10)), translate 20 0 (drawSquares (n - 1))]