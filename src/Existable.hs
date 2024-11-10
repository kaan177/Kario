{-# LANGUAGE NamedFieldPuns #-}
module Existable where
import Model (ShouldExist (Exist, RemoveIn), Enemy (enemyExist), PowerUp (powerShouldExist), Kario (karioExist), LevelState (kario))

class Existable a where
  getExistance :: a -> ShouldExist
  setExistance :: a -> ShouldExist -> a
  handleExistence :: Float -> [a] -> [a]
  handleExistence secs = foldr f []
    where f existable existables = case getExistance existable of
                          Exist      -> existable : existables
                          RemoveIn 0 -> existables
                          RemoveIn x -> setExistance existable (RemoveIn (x-secs)) : existables


instance Existable Enemy where
  getExistance = enemyExist
  setExistance e exist = e{enemyExist = exist}  

instance Existable PowerUp where
  getExistance = powerShouldExist
  setExistance p exist = p{powerShouldExist = exist}

instance Existable Kario where
  getExistance = karioExist
  setExistance k exist = k{karioExist = exist}
