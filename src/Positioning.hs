module Positioning where

import Collision (Collidable (getBox, updateBox))
import Model

--tried to make a typeclass, but ended up not being necessary, so handy helper functions for positioning are in this file

updateBoxPos :: Position -> Hitbox -> Hitbox
updateBoxPos newPos box = box{pos = newPos}

getPos :: (Collidable a) => a -> Position
getPos = pos . getBox

updatePos :: (Collidable a) => a -> Position -> a
updatePos col newPos = updateBox (updateBoxPos newPos (getBox col)) col
