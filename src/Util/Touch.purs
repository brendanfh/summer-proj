module Util.Touch where

import Prelude
import Control.Monad.Eff.Ref

import Types

foreign import setupTouch :: forall e. Ref TouchState -> Number -> Number -> EffGame e Unit


initialTouchState :: TouchState
initialTouchState = []

initialize :: forall e. Number -> Number -> EffGame e ( Ref TouchState )
initialize width height = do
    touchRef <- newRef initialTouchState
    setupTouch touchRef width height
    pure touchRef
    
updateTouches :: forall e. Ref Game -> EffGame e Unit
updateTouches gameRef = do
    game <- readRef gameRef
    touches <- readRef game.touch
    let newTouches = map (\t -> t { justTouched = false }) touches
    
    writeRef game.touch newTouches