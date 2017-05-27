module Util.Unsafe.UUID where

import Prelude (Unit)

foreign import unsafeGenUUID :: forall e. Unit -> String