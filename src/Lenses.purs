module Lenses where

import Optic.Core (Lens', lens)

state :: forall r a. Lens' { state :: a | r } a
state = lens (\r -> r.state) (\r val -> r { state = val })

blocks :: forall r a. Lens' { blocks :: a | r } a
blocks = lens (\r -> r.blocks) (\r val -> r { blocks = val })

balls :: forall r a. Lens' { balls :: a | r } a
balls = lens (\r -> r.balls) (\r val -> r { balls = val })
