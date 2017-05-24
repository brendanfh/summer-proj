module Types where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Ref (Ref, REF)
import Graphics.Canvas (CANVAS)

import Util.Log (LOG)

foreign import data GAME :: Effect

type EffGame e a = Eff ( game :: GAME
                       , log :: LOG
                       , canvas :: CANVAS
                       , ref :: REF | e ) a

data GameState 
    = Playing
    | Paused
    | MainMenu
    | HighScoreMenu
    | SettingsMenu

type Game =
    { state :: GameState
    , objects :: Array GameObj
    
    , keyboard :: Ref Keyboard
    , touch :: Ref TouchState
    }

type GameObject a = { x :: Number, y :: Number | a }

data GameObj
    = Block (GameObject ( health :: Int ))
    | Ball (GameObject ( vx :: Number, vy :: Number ))
    | PowerUp PowerUps
    
data PowerUps = NoPower



--INPUT TYPES
type Key = Int
type Keyboard = Array Key

type Touch = { x :: Number, y :: Number, identifier :: Int }
type TouchState = Array Touch