module Handlers.Destruction where

import Prelude
import Control.Monad.Eff.Ref
import Data.Array (concatMap)
import Optic.Core

import Lenses
import Types

removeDeadObjects :: forall e. Ref Game -> EffGame e Unit
removeDeadObjects gameRef = do
    game <- readRef gameRef
    let newObjs = concatMap isDead game.objects
    
    modifyRef gameRef (objects .~ newObjs)
    where
        isDead :: GameObj -> Array GameObj
        isDead b@(Block block)
            | block.health <= 0 = [ ]
            | otherwise = [ b ]
        isDead o = [ o ]