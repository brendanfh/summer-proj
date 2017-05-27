module Update (update) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Int (floor, toNumber)
import Data.Ord (min)
import Optic.Core

import Handlers.Collision as M
import Handlers.Destruction as M
import Handlers.Movement as M
import Lenses
import Types
import Util.Touch as M
import Util.Unsafe.UUID (unsafeGenUUID)

update' :: forall e. Number -> GameState -> Ref Game -> EffGame e Unit
update' time Playing gameRef = do
    foreachE [ addBlock
             , movementAndCollision time
             , M.removeDeadObjects
             , M.updateTouches
             ] $ \action -> do
                action gameRef

update' _ _ _ = pure unit

addBlock :: forall e. Ref Game -> EffGame e Unit
addBlock gameRef = do
    game <- readRef gameRef
    touches <- readRef game.touch
    
    foreachE touches $ \touch -> do
        ifM (pure touch.justTouched)
            (do
                let nBlock = Block { x : touch.x, y : touch.y, health : 100, id : unsafeGenUUID unit }
                
                modifyRef gameRef (objects <>~ [ nBlock ])
            )
            (pure unit)
            
movementAndCollision :: forall e. Number -> Ref Game -> EffGame e Unit
movementAndCollision time gameRef = do
    game <- readRef gameRef
    let dt = game.deltaTime
        timeScale = 0.002
        
        numberOfCycles = min (floor ((dt + time) / timeScale)) 100
    
    forE 0 numberOfCycles $ \i -> do
        M.moveObjects timeScale gameRef
        M.checkCollisions timeScale gameRef
    
    modifyRef gameRef (deltaTime .~ ((dt + time) - (toNumber numberOfCycles) * timeScale))
    

update :: forall e. Number -> Ref Game -> EffGame e Unit
update time gameRef = do
    game <- readRef gameRef
    let state = game.state
    update' time state gameRef