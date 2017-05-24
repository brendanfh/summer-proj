module Main where

import Prelude
import Control.Monad.Eff.Ref (Ref, newRef, readRef)
import Data.Tuple (Tuple(..))

import Globals as G
import Types
import Update (update)
import Util.Keyboard as K
import Util.Log (logStrLn, logLn)
import Util.RequestAnimFrame (requestAnimationFrame)
import Util.Touch as T
import View (view, setupView)
import Util.Rect (rect, intersectsWithAngle)

initial :: forall e. EffGame e (Ref Game)
initial = do
    keyboard <- K.initialize
    touch <- T.initialize G.width G.height
    let initialState =
            { state : Playing
            , objects : [ Ball { x : 0.0, y : 0.0, vx : 10.0, vy : 10.0 }
                        , Block { x : 100.0, y : 75.0, health : 10 }]
            , keyboard : keyboard
            , touch : touch
            }
    gameRef <- newRef initialState
    pure gameRef
    

main :: forall e. EffGame e Unit
main = do
    logStrLn "Loading..."
    logLn $ (intersectsWithAngle (rect 5.0 0.0 10.0 10.0) (rect 3.0 3.0 3.0 3.0))
    logLn $ (intersectsWithAngle (rect 3.0 3.0 3.0 3.0) (rect 5.0 0.0 10.0 10.0))
    
    Tuple canvas ctx <- setupView "gameCanvas" G.width G.height
    
    gameRef <- initial
    
    let loop time = do
            update time gameRef
            
            currState <- readRef gameRef
            view ctx currState
            
            requestAnimationFrame loop
    
    logStrLn "Starting the loop"
    requestAnimationFrame loop
