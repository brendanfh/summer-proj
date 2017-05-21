module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas as C
import Partial.Unsafe (unsafePartial)

main :: forall e. Eff ( canvas :: C.CANVAS | e ) Unit
main = unsafePartial $ do
    Just canvas <- C.getCanvasElementById "gameCanvas"
    ctx <- C.getContext2D canvas
    
    _ <- C.setFillStyle "black" ctx
    _ <- C.fillRect ctx { x : 0.0, y : 0.0, w : 600.0, h : 400.0 }
    
    pure unit
