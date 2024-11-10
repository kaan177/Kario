{-# language NamedFieldPuns #-}

module Collision where
import Model
import Data.Maybe(mapMaybe)

-- | Collidable type class
class Collidable a where
    getBox    :: a -> Hitbox
    updateBox :: Hitbox -> a -> a  

instance Collidable Kario where
    getBox Kario{karHitbox} = karHitbox
    updateBox newbox kario = kario{karHitbox = newbox} 

instance Collidable Platform where
    getBox (Ground hitbox)               = hitbox
    getBox (Brick hitbox)                = hitbox
    getBox (BreakBrick hitbox _)         = hitbox
    getBox (ItemBox hitbox _)            = hitbox
    getBox (EmptyItemBox hitbox)         = hitbox
    updateBox newBox (Ground _)          = Ground newBox      
    updateBox newBox (Brick _)           = Brick newBox
    updateBox newBox (BreakBrick _ shEx) = BreakBrick newBox shEx     
    updateBox newBox (ItemBox _ pt)      = ItemBox newBox pt            
    updateBox newBox (EmptyItemBox _)    = EmptyItemBox newBox            

instance Collidable Enemy where
    getBox = enemyBox
    updateBox newBox e = e{enemyBox = newBox}

instance Collidable Camera where
    getBox (Camera box _)      = box
    updateBox newBox (Camera _ vel) = Camera newBox vel

instance Collidable FlagPole where
    getBox (FlagPole hitbox) = hitbox
    updateBox newBox (FlagPole _)    = FlagPole newBox

instance Collidable Coin where
    getBox (Coin hitbox _ _) = hitbox
    updateBox newBox (Coin _ a b)    = Coin newBox a b
--------------------------------------------------------------------------------------------------------------
--{COLLISION FUNCTIONS}

getOverlaps :: (Collidable a, Collidable b) => a -> [b] -> [Overlap]
getOverlaps subject = mapMaybe (getOverlap (getBox subject) . getBox)

--calculates the intersecting part of the hitboxes, useful for more advanced collision detection for movement
getOverlap :: Hitbox -> Hitbox -> Maybe Overlap
getOverlap b1@(Hitbox (x1, y1) w1 h1) b2@(Hitbox (x2, y2) w2 h2) | isOverlapping b1 b2 = Just $ Hitbox (xmin, ymin) (xmax - xmin) (ymax - ymin)
                                                                 | otherwise           = Nothing
  where
    xmin = max x1 x2
    xmax = min (x1 + w1) (x2 + w2)
    ymin = max y1 y2
    ymax = min (y1 + h1) (y2 + h2)

isOverlapping :: Hitbox -> Hitbox -> Bool
isOverlapping hitbox1 hitbox2 = any (isWithin hitbox1) (hitboxToPoints hitbox2) || any (isWithin hitbox2) (hitboxToPoints hitbox1)

isWithin :: Hitbox -> Position -> Bool
isWithin Hitbox{pos = (hx, hy), width = w, height = h} (px, py) =
    px >= hx - (1/2 * w) &&
    px <= hx + (1/2 * w) &&
    py >= hy - (1/2 * h) &&
    py <= hy + (1/2 * h)

hitboxToPoints :: Hitbox -> [Position]
hitboxToPoints Hitbox{pos = (x, y), width = w, height = h} = [(x - (1/2 * w),y - (1/2 * h)), (x + (1/2 * w), y - (1/2 * h)), (x - (1/2 * w), y + (1/2 * h)),  (x + (1/2 * w), y + (1/2 * h))]

isColliding :: (Collidable a, Collidable b) => a -> b -> Bool
isColliding a b = isOverlapping (getBox a) (getBox b)