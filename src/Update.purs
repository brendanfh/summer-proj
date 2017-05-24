module Update (update) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Traversable (for)
import Optic.Core

import Lenses
import Types
import Util.Log
import Util.Rect (rect, intersects)

update' :: forall e. Number -> GameState -> Ref Game -> EffGame e Unit
update' time Playing gameRef = do
    state <- readRef gameRef
    let objs = state.objects
    objs' <- for objs (updateObj time state)
    
    modifyRef gameRef (objects .~ objs')
    
update' _ _ _ = pure unit

updateObj :: forall e. Number -> Game -> GameObj -> EffGame e GameObj
updateObj time game (Block block) = pure (Block block)
updateObj time game (Ball ball) = do
    let nx = ball.x + ball.vx * time
        ny = ball.y + ball.vy * time
    pure $ Ball $ ball { x = nx, y = ny }
    
updateObj time game p@(PowerUp powerup) = pure p

update :: forall e. Number -> Ref Game -> EffGame e Unit
update time gameRef = do
    game <- readRef gameRef
    let state = game.state
    update' time state gameRef