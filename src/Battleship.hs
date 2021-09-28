module Battleship where
import Data.Bifunctor (first, second)
import Data.List (intersect)
type Pos  = (Int, Int);

data Dir = North | West | South | East deriving (Eq, Show, Read) -- direction of the ship

data Def  = Def String Int deriving (Eq, Show, Read)
data Inst = Inst Def Pos Dir deriving (Eq, Show, Read)


type FleetDef = [Def]
type Fleet    = [Inst]

-- Muito obrigado wikipedia
standardFleet :: FleetDef
standardFleet = [Def "porta-avioes" 5, Def "navio-tanque" 4, Def "contratorpedeiro" 3, Def "submarino" 2]

directions = [Battleship.North, Battleship.South, Battleship.East, Battleship.West]

unDef :: Def -> (String, Int)
unDef (Def a b) = (a,b)

unInst :: Inst -> ((Def, Pos), Dir)
unInst (Inst a b c) = ((a,b),c)



rotateCounterClockwise :: Inst -> Inst
rotateCounterClockwise  = (uncurry $ uncurry Inst) . second rotateDirCounterClockwise . unInst

rotateDirClockwise :: Dir -> Dir
rotateDirClockwise North = West
rotateDirClockwise East  = North
rotateDirClockwise South = East
rotateDirClockwise West  = South

rotateClockwise :: Inst -> Inst
rotateClockwise  = (uncurry $ uncurry Inst) . second rotateDirClockwise . unInst

rotateDirCounterClockwise :: Dir -> Dir
rotateDirCounterClockwise North = East
rotateDirCounterClockwise East  = South
rotateDirCounterClockwise South = West
rotateDirCounterClockwise West  = North

move :: (Pos -> Pos) -> Inst -> Inst
move m = (uncurry $ uncurry Inst) . (first $ second m) . unInst

getSegments :: Inst -> [Pos]
getSegments (Inst (Def _ size) startingPos direction) = take size $ iterate (getDisPlacement direction) startingPos

getDisPlacement :: Dir -> (Pos -> Pos) -- parenteses desnecessarios, mas indicativos
getDisPlacement North = second $ \x -> x - 1
getDisPlacement East  = first  $ \x -> x - 1
getDisPlacement South = second $ \x -> x + 1
getDisPlacement West  = first  $ \x -> x + 1

parseDir :: Char -> Maybe Dir
parseDir 'w' = Just Battleship.North
parseDir 'a' = Just Battleship.East
parseDir 's' = Just Battleship.South
parseDir 'd' = Just Battleship.West
parseDir  _  = Nothing

getPos :: Inst -> Pos
getPos = snd . fst . unInst

checkIntersection :: Inst -> Inst -> Bool
checkIntersection i0 i1 = not $ null $ intersect (getSegments i0) (getSegments i1)

nameDef :: Def -> String
nameDef = fst . unDef

getDef :: Inst -> Def
getDef = fst . fst . unInst
nameInst :: Inst -> String
nameInst = nameDef . getDef