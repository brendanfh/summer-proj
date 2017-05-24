module Lenses where

import Optic.Core (Lens', lens)

state :: forall r a. Lens' { state :: a | r } a
state = lens (\r -> r.state) (\r val -> r { state = val })

objects :: forall r a. Lens' { objects :: a | r } a
objects = lens (\r -> r.objects) (\r val -> r { objects = val })
