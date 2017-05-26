module View (view, setupView) where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Graphics.Canvas as C
import Math (floor, pi)
import Partial.Unsafe (unsafePartial)

import Globals as G
import Types

clearScreen :: forall e. C.Context2D -> String -> EffGame e Unit
clearScreen ctx color = do
    _ <- C.setFillStyle color ctx
    _ <- C.fillRect ctx { x : 0.0, y : 0.0, w : G.width, h : G.height }
    pure unit
    

view' :: forall e. C.Context2D -> GameState -> Game -> EffGame e Unit
view' ctx Playing game = do
    clearScreen ctx "#1f1f1f"
    
    touches <- readRef game.touch
    foreachE touches $ \t -> do
        _ <- C.setFillStyle "white" ctx
        _ <- C.fillRect ctx { x : t.x, y : t.y, w : 10.0, h : 10.0 }
        pure unit
        
    foreachE game.objects (viewObj ctx)
    
view' _ _ _ = pure unit

viewObj :: forall e. C.Context2D -> GameObj -> EffGame e Unit
viewObj ctx (Ball ball) = do
    _ <- C.setFillStyle "red" ctx
    _ <- C.beginPath ctx
    _ <- C.arc ctx { x : floor (ball.x + hbSize), y : floor (ball.y + hbSize), r :hbSize , start : 0.0, end : 2.0 * pi }
    _ <- C.closePath ctx
    _ <- C.fill ctx
    pure unit
    where
        hbSize = G.ballSize / 2.0
    
viewObj ctx (Block block) = do
    _ <- C.setFillStyle "green" ctx
    _ <- C.fillRect ctx { x : block.x, y : block.y, w : G.blockSize, h : G.blockSize }
    _ <- C.setFillStyle "black" ctx
    _ <- C.fillText ctx (show block.health) (block.x + 8.0) (block.y + 20.0)
    pure unit
    
viewObj ctx _ = pure unit

view :: forall e. C.Context2D -> Game -> EffGame e Unit
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
    
    _ <- C.setFont "12pt Consolas" ctx
    
    pure $ Tuple canvas ctx
    