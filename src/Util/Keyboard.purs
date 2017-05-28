module Util.Keyboard where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref
import Data.Array (filter)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))

import Types (GAME, EffGame, Game, Keyboard, Key)

foreign import onKeyDown :: forall e. (Int -> Eff ( game :: GAME | e ) Unit) -> Eff ( game :: GAME | e ) Unit

foreign import onKeyUp :: forall e. (Int -> Eff ( game :: GAME | e ) Unit) -> Eff ( game :: GAME | e ) Unit

initialState :: Keyboard
initialState = []

initialize :: forall e. EffGame e (Ref Keyboard)
initialize = do
    keyRef <- newRef initialState
    onKeyDown (\code ->
        modifyRef keyRef ((<>) [ Tuple code true ])
    )
    onKeyUp (\code ->
        modifyRef keyRef (filter (\(Tuple c _) -> c /= code))
    )
    pure keyRef

updateKeyboard :: forall e. Ref Game -> EffGame e Unit
updateKeyboard gameRef = do
    game <- readRef gameRef
    keys <- readRef game.keyboard
    
    let newKeys = map (\(Tuple code _) -> Tuple code false) keys
    writeRef game.keyboard newKeys

isDown :: forall e. Ref Keyboard -> Int -> EffGame e Boolean
isDown keyRef keyCode = do
    keyState <- readRef keyRef
    pure $ isDown' keyState keyCode
    
isDown' :: Keyboard -> Int -> Boolean
isDown' keyboard keyCode = foldl (\acc (Tuple c _) -> acc || (c == keyCode)) false keyboard

isJustDown :: forall e. Ref Keyboard -> Int -> EffGame e Boolean
isJustDown keyRef keyCode = do
    keyState <- readRef keyRef
    pure $ isJustDown' keyState keyCode
    
isJustDown' :: forall e. Keyboard -> Int -> Boolean
isJustDown' keyboard keyCode = foldl (\acc (Tuple c jd) -> acc || ((c == keyCode) && jd)) false keyboard 
