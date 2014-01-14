import Control.Monad (filterM, forM_)
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = R | U | L | D deriving (Show, Eq)
data Action = Go Direction | Suck deriving (Show, Eq)

type MapFunc a = a -> Direction -> a
data State a b = State { getLocation :: a, getDirt :: (Set a), getMem :: b, getScore :: Int } deriving (Show, Eq)

data SimpleWorld = LeftSquare | RightSquare deriving (Show, Eq, Ord)

data Strategy a b = Strategy { decide :: b -> a -> Bool -> (Action, b),
                               initialmem :: b }

data Environment a b = Environment { godir :: (MapFunc a),
                                      scoreState :: (State a b) -> Int,
                                      scoreAction :: Action -> Int }

statelessStrategy :: (a -> Bool -> Action) -> (Strategy a ())
statelessStrategy decide = Strategy decide2 ()
  where decide2 () loc dirt = (decide loc dirt, ())


stepState :: (Ord a) => Environment a b -> State a b -> (Strategy a b) -> State a b
stepState env state@(State location dirt mem score) strategy =
  case decide strategy mem location (Set.member location dirt) of
    (Suck, newmem) -> State location (Set.delete location dirt) newmem (score + scoreState env state + scoreAction env Suck)
    (Go d, newmem) -> State (godir env location d) dirt newmem (score + scoreState env state + scoreAction env (Go d))

simulate :: (Ord a) => Environment a b -> (State a b) -> (Strategy a b) -> Int
simulate env initial strategy = getScore $ (iterate (\state -> stepState env state strategy) initial) !! 1000

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

memDecide :: (Set SimpleWorld) -> SimpleWorld -> Bool -> (Action, (Set SimpleWorld))
memDecide cleaned location True = (Suck, Set.insert location cleaned)
memDecide cleaned location@LeftSquare False = (if Set.member RightSquare cleaned then Suck else Go R, Set.insert location cleaned)
memDecide cleaned location@RightSquare False = (if Set.member LeftSquare cleaned then Suck else Go L, Set.insert location cleaned)

memStrategy :: Strategy SimpleWorld (Set SimpleWorld)
memStrategy = Strategy { decide = memDecide, initialmem = Set.empty }

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

penalizeMotion :: Action -> Int
penalizeMotion (Go _) = -1
penalizeMotion Suck = 0

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
  let env = Environment { godir = simpleGo,
                          scoreState = (\state -> -(Set.size (getDirt state))),
                          scoreAction = penalizeMotion }
  let strat = memStrategy --statelessStrategy reflexStrategy
  let squares = [LeftSquare, RightSquare]
  forM_
    (cartesian (powerList squares) squares)
    (\(dirt, loc) -> do
        let initial = (State loc (Set.fromList dirt) (initialmem strat) 0)
        print initial
        print (simulate env initial strat))
