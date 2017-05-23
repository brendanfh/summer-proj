module Util.Touch where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref

import Types

foreign import setupTouch :: forall e. Ref TouchState -> Number -> Number -> Eff ( game :: GAME, ref :: REF | e ) Unit


initialTouchState :: TouchState
initialTouchState = []

initialize :: forall e. Number -> Number -> Eff ( game :: GAME, ref :: REF | e ) ( Ref TouchState )
initialize width height = do
    touchRef <- newRef initialTouchState
    setupTouch touchRef width height
    pure touchRef
    