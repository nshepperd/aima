import System.Random (Random, StdGen, RandomGen, random, randomR, mkStdGen)
import Control.Monad (filterM, forM_)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (find)

data Direction = U | R | D | L deriving (Show, Eq, Ord, Enum, Bounded)
data Action = Go Direction | Suck deriving (Show, Eq)

randomChoice :: (RandomGen g) => [a] -> g -> (a, g)
randomChoice xs gen = (xs !! n, newgen)
  where (n, newgen) = randomR (0, length xs - 1) gen

instance Random Direction where
  randomR (lo, hi) = randomChoice [lo..hi]
  random = randomChoice [minBound..maxBound]

type MapFunc a = a -> Direction -> a
data State a b = State { getLocation :: a, getDirt :: (Set a), getMem :: b, getScore :: Int } deriving (Show, Eq)

data Strategy a b = Strategy { decide :: b -> a -> Bool -> (Action, b),
                               initialmem :: b }

data Environment a b = Environment { godir :: (MapFunc a),
                                      scoreState :: (State a b) -> Int,
                                      scoreAction :: Action -> Int }

statelessStrategy :: (a -> Bool -> Action) -> (Strategy a ())
statelessStrategy decide = Strategy decide2 ()
  where decide2 () loc dirt = (decide loc dirt, ())

randomStrategy :: (StdGen -> a -> Bool -> (Action, StdGen)) -> (Strategy a StdGen)
randomStrategy decide = Strategy decide (mkStdGen 7)

stepState :: (Ord a) => Environment a b -> State a b -> (Strategy a b) -> State a b
stepState env state@(State location dirt mem score) strategy =
  case decide strategy mem location (Set.member location dirt) of
    (Suck, newmem) -> State location (Set.delete location dirt) newmem (score + scoreState env state + scoreAction env Suck)
    (Go d, newmem) -> State (godir env location d) dirt newmem (score + scoreState env state + scoreAction env (Go d))

simulate :: (Ord a) => Environment a b -> (State a b) -> (Strategy a b) -> Int
simulate env initial strategy = getScore $ (iterate (\state -> stepState env state strategy) initial) !! 1000

dsimulate :: (Ord a, Show a, Show b) => Environment a b -> (State a b) -> (Strategy a b) -> IO ()
dsimulate env initial strategy = mapM_ print $ take 100 (iterate (\state -> stepState env state strategy) initial)

-- utilities
powerList :: [a] -> [[a]]
powerList = filterM (const [True, False])

cartesian :: [a] -> [b] -> [(a, b)]
cartesian xs ys = do
  x <- xs
  y <- ys
  return (x, y)

-- simple world
data SimpleWorld = LeftSquare | RightSquare deriving (Show, Eq, Ord)

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
  ".oooo..o",
  "..oo....",
  "o....oo."
  ]

isclear :: (Int, Int) -> Bool
isclear (x,y) = minimap!!y!!x == '.'

miniGo :: MapFunc (Int, Int)
miniGo (x, y) R = if x + 1 < length (minimap !! y) && isclear (x+1, y) then (x + 1, y) else (x, y)
miniGo (x, y) L = if x - 1 >= 0                    && isclear (x-1, y) then (x - 1, y) else (x, y)
miniGo (x, y) D = if y + 1 < length minimap        && isclear (x, y+1) then (x, y + 1) else (x, y)
miniGo (x, y) U = if y - 1 >= 0                    && isclear (x, y-1) then (x, y - 1) else (x, y)

unknownStrategy :: (Int,Int) -> Bool -> Action
unknownStrategy _ True = Suck
unknownStrategy _ False = Go R

stochDecide :: (RandomGen g) => (g -> a -> Bool -> (Action, g))
stochDecide gen loc True = (Suck, gen)
stochDecide gen loc False = let (d, newgen) = random gen in (Go d, newgen)

stochStrategy = randomStrategy stochDecide

penalizeMotion :: Action -> Int
penalizeMotion (Go _) = -1
penalizeMotion Suck = 0

-- stateful searching strategy

leftof :: Direction -> Direction
leftof U = L
leftof R = U
leftof D = R
leftof L = D
revof :: Direction -> Direction
revof = leftof . leftof
rightof :: Direction -> Direction
rightof = leftof . leftof . leftof
todx :: Direction -> (Int, Int)
todx U = (0, -1)
todx R = (1, 0)
todx D = (0, 1)
todx L = (-1, 0)

stepdir :: Direction -> (Int, Int) -> (Int, Int)
stepdir d (x,y) = let (dx, dy) = todx d in (x+dx, y+dy)

cleverStrategy :: Strategy (Int,Int) ([Direction], Maybe (Int, Int), Set (Int,Int))
cleverStrategy = Strategy cleverDecide cleverMem
cleverDecide (stack, prev, visited) loc True = (Suck, (stack, prev, visited))
cleverDecide (stack, prev, visited) loc False = case viable newvisit of
                                                    Just d -> (Go d, (d:newstack, Just loc, newvisit))
                                                    Nothing -> if null newstack then
                                                                 (Suck,
                                                                  (newstack, Nothing, newvisit))
                                                               else
                                                                 (Go (revof (head newstack)),
                                                                  (tail newstack, Nothing, newvisit))
  where
    viable :: Set (Int,Int) -> Maybe Direction
    viable vis = find (\d -> not (Set.member (stepdir d loc) vis)) [minBound..maxBound]
    newvisit1 = Set.insert loc visited
    (newstack, newvisit) = if not (null stack) && prev == Just loc then
                             (tail stack, Set.insert (stepdir (head stack) loc) newvisit1)
                           else
                             (stack, newvisit1)

cleverMem = ([], Nothing, Set.empty)

main :: IO ()
main = do
  let env = Environment { godir = miniGo,
                          scoreState = (\state -> -(Set.size (getDirt state))),
                          scoreAction = const 0 }
  let strategy = cleverStrategy
  let squares = filter isclear $ cartesian [0..length (head minimap) - 1] [0..length minimap - 1]
  forM_
    (cartesian ([squares]) squares)
    (\(dirt, loc) -> do
        let initial = (State loc (Set.fromList dirt) (initialmem strategy) 0)
        print initial
        print (simulate env initial strategy))
  -- let env = Environment { godir = simpleGo,
  --                         scoreState = (\state -> -(Set.size (getDirt state))),
  --                         scoreAction = penalizeMotion }
  -- let strat = memStrategy --statelessStrategy reflexStrategy
  -- let squares = [LeftSquare, RightSquare]
  -- forM_
  --   (cartesian (powerList squares) squares)
  --   (\(dirt, loc) -> do
  --       let initial = (State loc (Set.fromList dirt) (initialmem strat) 0)
  --       print initial
  --       print (simulate env initial strat))
