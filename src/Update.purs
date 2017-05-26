module Update (update) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Handlers.Collision as M
import Handlers.Destruction as M
import Handlers.Movement as M
import Types

update' :: forall e. Number -> GameState -> Ref Game -> EffGame e Unit
update' time Playing gameRef = do
    foreachE [ M.moveObjects time
             , M.checkCollisions time
             , M.removeDeadObjects
             ] $ \action -> do
                action gameRef

update' _ _ _ = pure unit

update :: forall e. Number -> Ref Game -> EffGame e Unit
update time gameRef = do
    game <- readRef gameRef
    let state = game.state
    update' time state gameRef