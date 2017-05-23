module Util.Keyboard where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref
import Data.Array (filter)
import Data.Foldable (foldl)

import Types (GAME, Keyboard, Key)

foreign import onKeyDown :: forall e. (Key -> Eff ( game :: GAME | e ) Unit) -> Eff ( game :: GAME | e ) Unit

foreign import onKeyUp :: forall e. (Key -> Eff ( game :: GAME | e ) Unit) -> Eff ( game :: GAME | e ) Unit

initialState :: Keyboard
initialState = []

initialize :: forall e. Eff ( game :: GAME, ref :: REF | e ) (Ref Keyboard)
initialize = do
    keyRef <- newRef initialState
    onKeyDown (\code ->
        modifyRef keyRef ((<>) [ code ])
    )
    onKeyUp (\code ->
        modifyRef keyRef (filter ((/=) code))
    )
    pure keyRef

isDown :: forall e. Ref Keyboard -> Key -> Eff ( ref :: REF, game :: GAME | e ) Boolean
isDown keyRef key = do
    keyState <- readRef keyRef
    pure $ isDown' keyState key
    
isDown' :: Keyboard -> Key -> Boolean
isDown' keyboard key = foldl (\acc val -> acc || (val == key)) false keyboard