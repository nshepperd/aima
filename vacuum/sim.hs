import Control.Monad (filterM, forM_)
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = R | U | L | D deriving (Show, Eq)
data Action = Go Direction | Suck deriving (Show, Eq)

type MapFunc a = a -> Direction -> a
data State a b = State { getLocation :: a, getDirt :: (Set a), getMem :: b } deriving (Show, Eq)

data SimpleWorld = LeftSquare | RightSquare deriving (Show, Eq, Ord)

data Strategy a b = SWM { decide :: b -> a -> Bool -> (Action, b),
                          initialmem :: b }

statelessStrategy :: (a -> Bool -> Action) -> (Strategy a ())
statelessStrategy decide = SWM decide2 ()
  where decide2 () loc dirt = (decide loc dirt, ())


stepState :: (Ord a) => (MapFunc a) -> State a b -> (Strategy a b) -> State a b
stepState godir (State location dirt mem) strategy =
  case decide strategy mem location (Set.member location dirt) of
    (Suck, newmem) -> State location (Set.delete location dirt) newmem
    (Go d, newmem) -> State (godir location d) dirt newmem

evaluate :: State a b -> Int
evaluate (State _ dirt _) = -(Set.size dirt)

simulate :: (Ord a) => (MapFunc a) -> (State a b) -> (Strategy a b) -> Int
simulate godir initial strategy = snd $ (iterate (\(state, score) -> (stepState godir state strategy, score + evaluate state)) (initial, 0)) !! 1000

-- utilities
powerList :: [a] -> [[a]]
powerList = filterM (const [True, False])

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- simple world
simpleGo :: MapFunc SimpleWorld
simpleGo _ R = RightSquare
simpleGo _ L = LeftSquare
simpleGo s _ = s

reflexStrategy :: SimpleWorld -> Bool -> Action
reflexStrategy _ True = Suck
reflexStrategy LeftSquare _ = Go R
reflexStrategy RightSquare _ = Go L

-- unknown map world
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

main :: IO ()
main = do
  -- let strat = statelessStrategy unknownStrategy
  -- let squares = [(0,0), (1,0), (2,0)]
  -- forM_
  --   (cartesian (powerList squares) squares)
  --   (\(dirt, loc) -> do
  --       let initial = (State loc (Set.fromList dirt) (initialmem strat))
  --       print initial
  --       print (simulate miniGo initial strat))
  let strat = statelessStrategy reflexStrategy
  let squares = [LeftSquare, RightSquare]
  forM_
    (cartesian (powerList squares) squares)
    (\(dirt, loc) -> do
        let initial = (State loc (Set.fromList dirt) (initialmem strat))
        print initial
        print (simulate simpleGo initial strat))
