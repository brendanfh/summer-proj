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
    
    let collisions = detectCollisions game.objects
    handleCollisions time gameRef collisions
    
detectCollisions :: forall e. Array GameObj -> Array Collision
detectCollisions objects = concatMap (coll objects) objects
    where
        coll :: Array GameObj -> GameObj -> Array Collision
        coll objs (Ball ball) =
            let ballRect = rect ball.x ball.y G.ballSize G.ballSize
                (Triple collided coords collObj) =
                    foldl
                        (collides ballRect)
                        (Triple false (Coords 0.0 0.0) (Ball ball))
                        objs
                        
            in if collided then
                [ Triple (Ball ball) collObj coords ]
            else
                [ ]
            
        coll _ _ = []
        
        collides :: Rect -> Triple Boolean Coords GameObj -> GameObj -> Triple Boolean Coords GameObj
        collides ballRect acc@(Triple hit _ _) b@(Block block) = if hit then acc else tupleToTriple potCol b
            where
                potCol :: Tuple Boolean Coords
                potCol = intersectsWithAngle ballRect (rect block.x block.y G.blockSize G.blockSize)
        
        collides ballRect acc@(Triple hit _ _) w@(Wall wall) = if hit then acc else tupleToTriple potCol w
            where
                potCol :: Tuple Boolean Coords
                potCol = intersectsWithAngle ballRect (rect wall.x wall.y wall.w wall.h)
                
        collides _ acc _ = acc
        
handleCollisions :: forall e. Number -> Ref Game -> Array Collision -> EffGame e Unit
handleCollisions time gameRef collisions = do
    foreachE collisions $ \c -> do 
        handleCollision c
    
    where
        handleCollision :: forall e. Collision -> EffGame e Unit
        handleCollision (Triple (Ball ball) (Block block) (Coords angle_x angle_y)) = do
            let newBall | angle_x < angle_y = ball { y = ball.y - ball.vy * time
                                                   , x = ball.x - ball.vx * time
                                                   , vy = -ball.vy }
                        | otherwise = ball { x = ball.x - ball.vx * time
                                           , y = ball.y - ball.vy * time
                                           , vx = -ball.vx }
                
                newBlock = block { health = block.health - 1 }
            
            setObjectById newBall.id (Ball newBall)
            setObjectById newBlock.id (Block newBlock)
            
        handleCollision (Triple (Ball ball) (Wall wall) (Coords angle_x angle_y)) = do
            let newBall | angle_x / wall.w < angle_y / wall.h = ball { y = ball.y - ball.vy * time
                                                                     , x = ball.x - ball.vx * time
                                                                     , vy = -ball.vy }
                        | otherwise = ball { x = ball.x - ball.vx * time
                                           , y = ball.y - ball.vy * time
                                           , vx = -ball.vx }
                
            setObjectById newBall.id (Ball newBall)
        
        handleCollision _ = pure unit
        
        setObjectById :: forall e. String -> GameObj -> EffGame e Unit
        setObjectById id obj = do
            game <- readRef gameRef
            let func :: GameObj -> GameObj
                func (Block o) = if o.id == id then obj else Block o
                func (Ball o) = if o.id == id then obj else Ball o
                func o = o
                
                newObjs = map func game.objects
            
            modifyRef gameRef (objects .~ newObjs)