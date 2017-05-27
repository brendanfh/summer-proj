module Main where

import Prelude
import Control.Monad.Eff.Ref (Ref, newRef, readRef)
import Data.Tuple (Tuple(..))

import Globals as G
import Types
import Update (update)
import Util.Keyboard as K
import Util.Log (logStrLn)
import Util.RequestAnimFrame (requestAnimationFrame)
import Util.Touch as T
import Util.Unsafe.UUID (unsafeGenUUID)
import View (view, setupView)

initial :: forall e. EffGame e (Ref Game)
initial = do
    keyboard <- K.initialize
    touch <- T.initialize G.width G.height
    let initialState =
            { state : Playing
            , objects : [ Wall { x : -20.0, y : -20.0, w : 20.0, h : G.height + 40.0, id : unsafeGenUUID unit }
                        , Wall { x : -20.0, y : -20.0, w : G.width + 40.0, h : 20.0, id : unsafeGenUUID unit }
                        , Wall { x : G.width, y : -20.0, w : 20.0, h : G.height + 40.0, id : unsafeGenUUID unit }
                        , Wall { x : -20.0, y : G.height, w : G.width + 40.0, h : 20.0, id : unsafeGenUUID unit }
                        , Ball { x : 0.0, y : 0.0, vx : 300.0, vy : 300.0, id : unsafeGenUUID unit }
                        , Ball { x : 200.0, y : 0.0, vx : -200.0, vy : 250.0, id : unsafeGenUUID unit }
                        , Ball { x : 108.0, y : 300.0, vx : 240.0, vy : -500.0, id : unsafeGenUUID unit }
                        , Block { x : 100.0, y : 75.0, health : 2, id : unsafeGenUUID unit }
                        , Block { x : 50.0, y : 125.0, health : 1, id : unsafeGenUUID unit } ]
            , keyboard : keyboard
            , touch : touch
            , deltaTime : 0.0
            }
    gameRef <- newRef initialState
    pure gameRef
    

main :: forall e. EffGame e Unit
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
