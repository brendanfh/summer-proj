module Main where

import Prelude
import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Ref
import Data.Tuple (Tuple(..))
import Graphics.Canvas as C

import Globals as G
import Types
import Update (update)
import Util.Keyboard as K
import Util.Log
import Util.RequestAnimFrame (requestAnimationFrame)
import Util.Touch as T
import View (view, setupView)

initial :: forall e. Eff ( game :: GAME, ref :: REF | e ) (Ref Game)
initial = do
    keyboard <- K.initialize
    touch <- T.initialize G.width G.height
    let initialState =
            { state : Playing
            , blocks : []
            , balls : []
            , powerups : []
            , keyboard : keyboard
            , touch : touch
            }
    gameRef <- newRef initialState
    pure gameRef
    

main :: forall e. Eff ( log :: LOG, game :: GAME, ref :: REF, canvas :: C.CANVAS | e ) Unit
main = do
    logStrLn "Loading..."
    
    Tuple canvas ctx <- setupView "gameCanvas" G.width G.height
    
    gameRef <- initial
    
    let loop time = do
            update time gameRef
            
            currState <- readRef gameRef
            view ctx currState
            
            requestAnimationFrame loop
    
    logStrLn "Starting the loop"
    requestAnimationFrame loop
