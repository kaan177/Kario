module Positioning where

import Collision (Collidable (getBox, updateBox))
import Model

--Typeclass for position

updateBoxPos :: Position -> Hitbox -> Hitbox
updateBoxPos newPos box = box{pos = newPos}

getPos :: (Collidable a) => a -> Position
getPos = pos . getBox

updatePos :: (Collidable a) => a -> Position -> a
updatePos col newPos = updateBox (updateBoxPos newPos (getBox col)) col
