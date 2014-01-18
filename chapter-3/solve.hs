import Geometry
import Data.Ratio (Rational)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (><))

seqtail = Seq.drop 1
seqhead = (flip Seq.index) 0

type PolySpace = Rational
type PolyState = Vertex PolySpace
type PolyAction = (PolyState, PolyState)

terrain :: [Polygon PolySpace]
terrain = [[(1,0),(1,1),(0,1)]]

polystart :: PolyState
polystart = (0,0)
polygoal :: PolyState
polygoal = (2,2)

polycost :: PolyState -> PolyAction -> Double
polycost _ _ = 1

-- cost ((x0,y0),(x1,y1)) = sqrt . fromRational $ (x1-x0)^2 + (y1-y0)^2

polyActions :: PolyState -> [PolyAction]
polyActions st = filter legal [(st,next) | next <- polygoal:(concat terrain), st /= next]
    where legal action = not $ or (map (p_intersects action) terrain)

polymove :: PolyState -> PolyAction -> PolyState
polymove _ (_,s) = s


------ Solving Machinery ------

data Problem a s = Problem { initial :: s,
                             actions :: s -> [a],
                             result :: s -> a -> s,
                             goal :: s -> Bool,
                             cost :: s -> a -> Double }

data Node a s = Node { getActions :: [a], getState :: s }

isgoalnode :: Problem a s -> Node a s -> Bool
isgoalnode problem (Node _ s) = goal problem s
movenode :: Problem a s -> Node a s -> a -> Node a s
movenode problem (Node as s) action = Node (action:as) (result problem s action)

type Frontier a s = Seq (Node a s)
type Closed s = Set s
type Solution a = [a]
search' :: (Ord s) => Problem a s -> Frontier a s -> Closed s -> Maybe (Solution a)
search' problem frontier closed = if Seq.null frontier then
                                      Nothing
                                  else
                                      let frontier' = seqtail frontier
                                          top = seqhead frontier
                                      in if isgoalnode problem top then
                                             Just $ getActions top
                                         else
                                             let closed' = Set.insert (getState top) closed
                                                 tovisit = filter (not . ((flip Set.member) closed') . getState . (movenode problem top)) (actions problem (getState top))
                                                 frontier'' = frontier' >< (Seq.fromList $ map (movenode problem top) tovisit)
                                             in search' problem frontier'' closed'

search :: (Ord s) => Problem a s -> Maybe (Solution a)
search problem = search' problem (Seq.singleton (Node [] (initial problem))) (Set.empty)


prob :: Problem PolyAction PolyState
prob = Problem { initial = polystart,
                 actions = polyActions,
                 result = polymove,
                 goal = (==polygoal),
                 cost = polycost }

main :: IO ()
main = do
  print $ search prob
