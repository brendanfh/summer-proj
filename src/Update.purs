module Update (update) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Types


update' :: forall e. Number -> GameState -> Ref Game -> Eff ( ref :: REF | e ) Unit
update' time Playing gameRef = do
    pure unit
update' _ _ _ = pure unit

update :: forall e. Number -> Ref Game -> Eff ( ref :: REF | e ) Unit
update time gameRef = do
    game <- readRef gameRef
    let state = game.state
    update' time state gameRef