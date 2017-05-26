module Handlers.Movement where

import Prelude
import Control.Monad.Eff.Ref
import Optic.Core

import Lenses
import Types

moveObjects :: forall e. Number -> Ref Game -> EffGame e Unit
moveObjects time gameRef = do
    game <- readRef gameRef
    let newObjs = map (moveObj time) game.objects
    
    modifyRef gameRef (objects .~ newObjs)
    
moveObj :: Number -> GameObj -> GameObj
moveObj time (Ball ball) = Ball nball
    where
        nball = ball { x = ball.x + ball.vx * time
                     , y = ball.y + ball.vy * time }
moveObj _ obj = obj
