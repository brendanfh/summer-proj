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
            , objects : [ Ball { x : 0.0, y : 0.0, vx : 100.0, vy : 100.0, id : 0 }
                        , Ball { x : 200.0, y : 0.0, vx : -100.0, vy : 110.0, id : 1 }
                        , Ball { x : 108.0, y : 300.0, vx : 0.0, vy : -180.0, id : 2 }
                        , Block { x : 100.0, y : 75.0, health : 2, id : 3 }
                        , Block { x : 50.0, y : 125.0, health : 1, id : 4 } ]
            , keyboard : keyboard
            , touch : touch
            }
    gameRef <- newRef initialState
    pure gameRef
    

main :: forall e. EffGame e Unit
main = do
    logStrLn "Loading..."
    logLn $ (intersectsWithAngle (rect 0.0 0.0 10.0 10.0) (rect 3.0 3.0 3.0 3.0))
    
    Tuple canvas ctx <- setupView "gameCanvas" G.width G.height
    
    gameRef <- initial
    
    let loop time = do
            update time gameRef
            
            currState <- readRef gameRef
            view ctx currState
            
            requestAnimationFrame loop
    
    logStrLn "Starting the loop"
    requestAnimationFrame loop
