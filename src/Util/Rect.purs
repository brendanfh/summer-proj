module Util.Rect
    ( Rect
    , Coords(..)
    , rect
    , intersects
    , intersectsWithAngle
    )
    where

import Prelude
import Data.Tuple (Tuple(..))
import Math (abs)

type Rect = { x :: Number, y :: Number, w :: Number, h :: Number }

newtype Coords = Coords { x :: Number, y :: Number }

instance showCoords :: Show Coords where
    show (Coords c) = "(" <> (show c.x) <> ", " <> (show c.y) <> ")"

rect :: Number -> Number -> Number -> Number -> Rect
rect x y w h = { x, y, w, h }

intersects :: Rect -> Rect -> Boolean
intersects r1 r2 = top && bottom && left && right
    where
        right = r1.x + r1.w >= r2.x
        left = r2.x + r2.w >= r1.x
        top = r1.y + r1.h >= r2.y
        bottom = r2.y + r2.h >= r1.y

intersectsWithAngle :: Rect -> Rect -> Tuple Boolean Coords
intersectsWithAngle r1 r2
    | intersects r1 r2 = Tuple true $ Coords { x : abs $ (r1.x + r1.w / 2.0) - (r2.x + r2.w / 2.0)
                                             , y : abs $ (r1.y + r1.h / 2.0) - (r2.y + r2.h / 2.0) }
    | otherwise = Tuple false $ Coords { x : 0.0, y : 0.0 }
