module View (view, setupView) where

import Prelude
import Control.Monad.Eff
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Canvas as C
import Partial.Unsafe (unsafePartial)

import Globals as G
import Types

clearScreen :: forall e. C.Context2D -> String -> Eff ( canvas :: C.CANVAS | e ) Unit
clearScreen ctx color = do
    _ <- C.setFillStyle color ctx
    _ <- C.fillRect ctx { x : 0.0, y : 0.0, w : G.width, h : G.height }
    pure unit
    

view' :: forall e. C.Context2D -> GameState -> Game -> Eff ( canvas :: C.CANVAS |  e ) Unit
view' ctx Playing game = do
    clearScreen ctx "black"
    
view' _ _ _ = pure unit

view :: forall e. C.Context2D -> Game -> Eff ( canvas :: C.CANVAS | e ) Unit
view ctx game = view' ctx game.state game

setupView ::
    forall e
     . String 
    -> Number
    -> Number 
    -> Eff ( canvas :: C.CANVAS | e ) (Tuple C.CanvasElement C.Context2D)
setupView canvasName width height = unsafePartial $ do
    Just canvas <- C.getCanvasElementById canvasName
    ctx <- C.getContext2D canvas
    
    _ <- C.setCanvasWidth width canvas
    _ <- C.setCanvasHeight height canvas
    
    pure $ Tuple canvas ctx
    