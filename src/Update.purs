module Update (update) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Array ((..))
import Data.Int (floor, toNumber)
import Data.Ord (min)
import Data.Traversable (for)
import Optic.Core

import Handlers.Collision as M
import Handlers.Destruction as M
import Handlers.Movement as M
import Lenses
import Types
import Util.Keyboard as M
import Util.Touch as M
import Util.Unsafe.UUID (unsafeGenUUID)

update' :: forall e. Number -> GameState -> Ref Game -> EffGame e Unit
update' time Playing gameRef = do
    foreachE [ addBlock
             , addMoreBlocksTEMP
             , movementAndCollision time
             , M.removeDeadObjects
             , M.updateTouches
             , M.updateKeyboard
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
            
addMoreBlocksTEMP :: forall e. Ref Game -> EffGame e Unit
addMoreBlocksTEMP gameRef = do
    game <- readRef gameRef
    ifM (M.isJustDown game.keyboard 32)
        (do
            newBlocks <- for (0..9) $ \i -> do
                pure $ Block { x : (toNumber i) * 34.0, y : 100.0, health : 50, id : unsafeGenUUID unit }
            
            modifyRef gameRef (objects <>~ newBlocks)
        )
        (pure unit)
            
movementAndCollision :: forall e. Number -> Ref Game -> EffGame e Unit
movementAndCollision time gameRef = do
    game <- readRef gameRef
    let dt = game.deltaTime
        timeScale = 0.002
        
        numberOfCycles = min (floor ((dt + time) / timeScale)) 100
    
    forE 0 (numberOfCycles) $ \i -> do
        M.moveObjects timeScale gameRef
        M.checkCollisions timeScale gameRef
    
    modifyRef gameRef (deltaTime .~ ((dt + time) - (toNumber numberOfCycles) * timeScale))
    

update :: forall e. Number -> Ref Game -> EffGame e Unit
update time gameRef = do
    game <- readRef gameRef
    let state = game.state
    update' time state gameRef