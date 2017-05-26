module Handlers.Collision where

import Prelude
import Control.Monad.Eff (foreachE)
import Control.Monad.Eff.Ref
import Data.Array (concatMap)
import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import Optic.Core

import Globals as G
import Lenses
import Types
import Util.Log
import Util.Rect (Coords(..), Rect, rect, intersectsWithAngle)

type Collision = Triple GameObj GameObj Coords


checkCollisions :: forall e. Number -> Ref Game -> EffGame e Unit
checkCollisions time gameRef = do
    game <- readRef gameRef
    
    collisions <- detectCollisions game.objects
    handleCollisions time gameRef collisions
    
detectCollisions :: forall e. Array GameObj -> EffGame e (Array Collision)
detectCollisions objects = pure $ concatMap (coll objects) objects
    where
        coll :: Array GameObj -> GameObj -> Array Collision
        coll objs (Ball ball) =
            let ballRect = rect ball.x ball.y G.ballSize G.ballSize
                (Triple collided coords block) =
                    foldl
                        (collides ballRect)
                        (Triple false (Coords { x : 0.0, y : 0.0 }) (Ball ball))
                        objs
                        
            in if collided then
                [ Triple (Ball ball) block coords ]
            else
                [ ]
            
        coll _ _ = []
        
        collides :: Rect -> Triple Boolean Coords GameObj -> GameObj -> Triple Boolean Coords GameObj
        collides ballRect acc@(Triple hit _ _) b@(Block block) = if hit then acc else tupleToTriple potCol b
            where
                potCol :: Tuple Boolean Coords
                potCol = intersectsWithAngle ballRect (rect block.x block.y G.blockSize G.blockSize)
                
        collides _ acc _ = acc
        
handleCollisions :: forall e. Number -> Ref Game -> Array Collision -> EffGame e Unit
handleCollisions time gameRef collisions = do
    foreachE collisions $ \c -> do 
        handleCollision c
        logStrLn "COLLISION"
    
    where
        handleCollision :: forall e. Collision -> EffGame e Unit
        handleCollision (Triple (Ball ball) (Block block) (Coords angle)) = do
            let newBall | angle.x < angle.y = ball { y = ball.y - ball.vy * time, vy = -ball.vy }
                        | otherwise = ball { x = ball.x - ball.vx * time, vx = -ball.vx }
                
                newBlock = block { health = block.health - 1 }
            
            setObjectById newBall.id (Ball newBall)
            setObjectById newBlock.id (Block newBlock)
            
            
        handleCollision _ = pure unit
        
        setObjectById :: forall e. Int -> GameObj -> EffGame e Unit
        setObjectById id obj = do
            game <- readRef gameRef
            let func :: GameObj -> GameObj
                func (Block o) = if o.id == id then obj else Block o
                func (Ball o) = if o.id == id then obj else Ball o
                func o = o
                
                newObjs = map func game.objects
            
            modifyRef gameRef (objects .~ newObjs)