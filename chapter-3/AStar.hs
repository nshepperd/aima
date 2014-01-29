module AStar (search) where
import Problem (Problem (..), Solution (Solution))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Queue.Class as Q
import Data.Queue.PQueue (PQueue)

import Debug.Trace (traceShow)

data Node a s = Node { getActions :: [a],
                       getState :: s,
                       getCost :: Double,
                       getHeuristic :: Double} deriving (Show, Eq)

instance (Eq a, Eq s) => Ord (Node a s) where
    one <= two = (getCost one + getHeuristic one) <= (getCost two + getHeuristic two)

type Heuristic s = s -> Double

movenode :: Problem a s -> Heuristic s -> Node a s -> a -> Node a s
movenode problem h (Node as s c _) action = let newstate = result problem s action in
                                          Node (action:as) newstate (c + cost problem s action) (h newstate)

unvisited :: (Ord s) => Problem a s -> Set s -> s -> [a]
unvisited problem closed state = filter (not . (flip Set.member closed) . (result problem state)) (actions problem state)

search :: (Eq a, Ord s, Show a, Show s) => Problem a s -> Heuristic s -> Maybe (Solution a s)
search problem h = search' problem h (Q.singleton (Node [] (initial problem) 0 (h $ initial problem))) (Set.empty)

type Frontier a s = PQueue (Node a s)
type Closed s = Set s
search' :: (Eq a, Ord s, Show a, Show s) => Problem a s -> Heuristic s -> Frontier a s -> Closed s -> Maybe (Solution a s)
search' problem h frontier closed = case Q.extract frontier of
                                      Just (top, frontier') ->
                                          if goal problem (getState top) then
                                              Just $ case top of { Node a s c _ -> Solution a s c }
                                          else if (not $ Set.member (getState top) closed) then
                                                   let closed' = Set.insert (getState top) closed
                                                       tovisit = unvisited problem closed' (getState top)
                                                       nodes = map (movenode problem h top) tovisit
                                                       frontier'' = Q.insertAll nodes frontier'
                                                   in search' problem h frontier'' closed'
                                               else
                                                   search' problem h frontier' closed
                                      Nothing -> Nothing
