module Sea where

import qualified Battleship

import Data.List (foldl')
import Data.Bifunctor (first, second)

data SeaTileState = Pristine | Bombed deriving (Show, Read, Eq)
data ShipTileState = ShipPresent Battleship.Inst | ShipAbsent deriving (Show, Read, Eq)
type SeaTile = (SeaTileState, ShipTileState)
data BombResult = Hit Battleship.Inst | ShipDestroyed Battleship.Inst | Miss deriving (Show, Read, Eq)
type Sea    = [[SeaTile]]
type Setup  = [[ShipTileState]]

type Pos    = Battleship.Pos -- adopt the name
type Bounds = (Int, Int)     -- same as Pos, but different meaning

standardBounds :: Bounds
standardBounds = (10, 10)

uMapIndex :: (a -> a) -> Int -> [a] -> [a] -- apply function at index
uMapIndex _ _ [] = error "Out of index"
uMapIndex f n (x:xs)
  | n < 0 = error "Out of index"
  | n > 0 = x:uMapIndex f (n-1) xs
  | otherwise = (f x):xs

gPos :: Int -> Int -> [[a]] -> a
gPos x y = (!! x) . (!! y)

uPos :: Int -> Int -> a -> [[a]] -> [[a]]
uPos x y v = uMapIndex (uMapIndex (\_ -> v) x) y

uPosMap :: Int -> Int -> (a -> a) -> [[a]] -> [[a]]
uPosMap x y f m = uPos x y (f $ gPos x y m) m 

dims :: [[a]] -> Bounds
dims m = (length $ head m, length m)

placeShip :: Battleship.Inst -> Setup -> Setup
placeShip ship setup = foldl' (flip ($)) setup $ fmap (placeShipSegment ship) $ filter (flip checkPosBounds $ dims setup) $ Battleship.getSegments ship

placeShipSegment :: Battleship.Inst -> Pos -> Setup -> Setup
placeShipSegment ship p = uncurry uPos p $ ShipPresent ship

placeFleet :: Battleship.Fleet -> Setup -> Setup
placeFleet fleet setup = foldl' (flip ($)) setup $ fmap placeShip fleet

checkBounds :: Battleship.Inst -> Bounds -> Bool
checkBounds inst bounds =
  let (e0, e1) = getEnds inst
  in  (checkPosBounds e0 bounds) && (checkPosBounds e1 bounds)
  where
    getEnds :: Battleship.Inst -> (Battleship.Pos, Battleship.Pos)
    getEnds inst = (head segment, last segment) where segment = Battleship.getSegments inst
    
checkPosBounds :: Battleship.Pos -> Bounds -> Bool
checkPosBounds (x,y) (lx, ly) = x >= 0 && y >= 0 && x < lx && y < ly


-- Sea.create
setupCreate :: Bounds -> Setup
setupCreate (l,h) = replicate h $ replicate l $ ShipAbsent

generateRealSea :: Setup -> Sea
generateRealSea = (fmap . fmap) ((,) Pristine)

bombPosOwned :: Pos -> Sea -> Maybe (Sea, BombResult)
bombPosOwned pos sea
  | not $ checkPosBounds pos $ dims sea = Nothing
  | alreadyBombed target = Nothing -- Fails if you bombed it a already so the game can stop 
  | otherwise            = case snd target of
    ShipAbsent -> Just (bombedSea, Miss)
    ShipPresent
     ship -> Just (bombedSea, if allAlreadyBombed $ Battleship.getSegments ship then ShipDestroyed ship else Hit ship)
    where 
      target = uncurry gPos pos sea
      
      alreadyBombed :: SeaTile -> Bool
      alreadyBombed = (== Bombed) . fst
      
      bombedSea = uncurry uPosMap pos (first $ \_ -> Bombed) sea
      
      allAlreadyBombed = all alreadyBombed . fmap ((flip $ uncurry gPos) bombedSea)