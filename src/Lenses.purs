module Lenses where

import Prelude (flip, ($))
import Optic.Core (Lens', lens)

composeLenses :: forall a b. a -> (a -> b) -> b
composeLenses = flip ($)
infixr 1 composeLenses as &

state :: forall r a. Lens' { state :: a | r } a
state = lens (\r -> r.state) (\r val -> r { state = val })

objects :: forall r a. Lens' { objects :: a | r } a
objects = lens (\r -> r.objects) (\r val -> r { objects = val })
