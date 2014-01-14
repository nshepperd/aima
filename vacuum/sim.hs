import Control.Monad (filterM, forM_)
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = R | U | L | D deriving (Show, Eq)
data Action = Go Direction | Suck deriving (Show, Eq)

type Strategy a = (a -> Bool -> Action)
type MapFunc a = a -> Direction -> a
data State a = State a (Set a) deriving (Show, Eq)

data SimpleWorld = LeftSquare | RightSquare deriving (Show, Eq, Ord)

data StrategyWithMemory a b = SWM { decide :: b -> a -> Bool -> (Action, b),
                                    initialmem :: b }

simpleGo :: MapFunc SimpleWorld
simpleGo _ R = RightSquare
simpleGo _ L = LeftSquare
simpleGo s _ = s

reflexStrategy :: SimpleWorld -> Bool -> Action
reflexStrategy _ True = Suck
reflexStrategy LeftSquare _ = Go R
reflexStrategy RightSquare _ = Go L

stepState :: (Ord a) => (MapFunc a) -> State a -> (a -> Bool -> Action) -> State a
stepState godir (State location dirt) strategy =
  case strategy location (Set.member location dirt) of
    Suck -> State location (Set.delete location dirt)
    Go d -> State (godir location d) dirt

evaluate :: State a -> Int
evaluate (State _ dirt) = -(Set.size dirt)

simulate :: (Ord a) => (MapFunc a) -> State a -> (a -> Bool -> Action) -> Int
simulate godir initial strategy = snd $ (iterate (\(state, score) -> (stepState godir state strategy, score + evaluate state)) (initial, 0)) !! 1000

powerList :: [a] -> [[a]]
powerList = filterM (const [True, False])

minimap :: [String]
minimap = [
  "..."
  ]

miniGo :: MapFunc (Int, Int)
miniGo (x, y) R = if x + 1 < length (minimap !! y) then (x + 1, y) else (x, y)
miniGo (x, y) L = if x - 1 >= 0 then (x - 1, y) else (x, y)
miniGo (x, y) D = if y + 1 < length minimap then (x, y + 1) else (x, y)
miniGo (x, y) U = if y - 1 >= 0 then (x, y - 1) else (x, y)

unknownStrategy :: (Int,Int) -> Bool -> Action
unknownStrategy _ True = Suck
unknownStrategy _ False = Go R

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = do
  x <- xs
  y <- ys
  return (x, y)

main :: IO ()
main = do
  forM_
    (cartesian (powerList [(0,0),(1,0),(2,0)]) [(0,0),(1,0),(2,0)])
    (\(dirt, loc) -> do
        let initial = (State loc (Set.fromList dirt))
        print initial
        print (simulate miniGo initial unknownStrategy))
  -- forM_ (powerList [LeftSquare, RightSquare]) (\dirt -> do
  --   forM_ [LeftSquare, RightSquare] (\loc -> do
  --     let initial = (State loc (Set.fromList dirt))
  --     print initial
  --     print (simulate simpleGo initial reflexStrategy)))
