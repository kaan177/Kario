module Collision where
import Model
import Data.Maybe(mapMaybe)

getOverlaps :: (Collidable a, Collidable b) => a -> [b] -> [Hitbox]
getOverlaps subject = mapMaybe (getOverlap (getBox subject) . getBox)

--calculates the intersecting part of the hitboxes, useful for more advanced collision detection for movement
getOverlap :: Hitbox -> Hitbox -> Maybe Hitbox
getOverlap b1@(Hitbox (x1, y1) w1 h1) b2@(Hitbox (x2, y2) w2 h2) | isOverlapping b1 b2 = Just $ Hitbox (xmin, xmax) (xmax - xmin) (ymax - ymin)
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