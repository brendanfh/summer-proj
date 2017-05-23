module Util.RequestAnimFrame where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Types (GAME)

foreign import requestAnimationFrame :: forall e. (Number -> Eff ( game :: GAME | e ) Unit) -> Eff ( game :: GAME | e ) Unit