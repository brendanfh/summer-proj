module Types where

import Prelude
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Ref (Ref)

foreign import data GAME :: Effect

data GameState 
    = Playing
    | Paused
    | MainMenu
    | HighScoreMenu
    | SettingsMenu

type Game =
    { state :: GameState
    , blocks :: Array Block
    , balls :: Array Ball
    , powerups :: Array PowerUp
    
    , keyboard :: Ref Keyboard
    , touch :: Ref TouchState
    }

type Block = {}
type Ball = {}
data PowerUp = NoPower



--INPUT TYPES
type Key = Int
type Keyboard = Array Key

type Touch = { x :: Number, y :: Number, identifier :: Int }
type TouchState = Array Touch