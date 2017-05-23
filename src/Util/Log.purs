module Util.Log where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)


foreign import data LOG :: Effect

foreign import logImpl :: forall e. String -> String -> Eff ( log :: LOG | e ) Unit

log :: forall a e. (Show a) => a -> Eff ( log :: LOG | e ) Unit
log a = logImpl (show a) ""

log' :: forall a e. (Show a) => a -> Eff ( log :: LOG | e ) a
log' a = do
    logImpl (show a) ""
    pure a
    
logLn :: forall a e. (Show a) => a -> Eff ( log :: LOG | e ) Unit
logLn a = logImpl (show a) "\n"

logLn' :: forall a e. (Show a) => a -> Eff ( log :: LOG | e ) a
logLn' a = do
    logImpl (show a) "\n"
    pure a
    
logStr :: forall a e. String -> Eff ( log :: LOG | e ) Unit
logStr str = logImpl str ""

logStrLn :: forall a e. String -> Eff ( log :: LOG | e ) Unit
logStrLn str = logImpl str "\n"