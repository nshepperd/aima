module UniformCost (search) where
import Problem (Problem (..), Solution (Solution))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Queue.Class as Q
import Data.Queue.PQueue (PQueue)

import Debug.Trace (traceShow)

data Node a s = Node { getActions :: [a],
                       getState :: s,
                       getCost :: Double } deriving (Show, Eq)

instance (Eq a, Eq s) => Ord (Node a s) where
    one <= two = (getCost one) <= (getCost two)


movenode :: Problem a s -> Node a s -> a -> Node a s
movenode problem (Node as s c) action = Node (action:as) (result problem s action) (c + cost problem s action)

unvisited :: (Ord s) => Problem a s -> Set s -> s -> [a]
unvisited problem closed state = filter (not . (flip Set.member closed) . (result problem state)) (actions problem state)

search :: (Eq a, Ord s, Show a, Show s) => Problem a s -> Maybe (Solution a s)
search problem = search' problem (Q.singleton (Node [] (initial problem) 0)) (Set.empty)

type Frontier a s = PQueue (Node a s)
type Closed s = Set s
search' :: (Eq a, Ord s, Show a, Show s) => Problem a s -> Frontier a s -> Closed s -> Maybe (Solution a s)
search' problem frontier closed = case Q.extract frontier of
                                    Just (top, frontier') ->
                                        if goal problem (getState top) then
                                            Just $ case top of { Node a s c -> Solution a s c }
                                        else if (not $ Set.member (getState top) closed) then
                                                 let closed' = Set.insert (getState top) closed
                                                     tovisit = unvisited problem closed' (getState top)
                                                     nodes = map (movenode problem top) tovisit
                                                     frontier'' = Q.insertAll nodes frontier'
                                                 in search' problem frontier'' closed'
                                             else
                                                 search' problem frontier' closed
                                    Nothing -> Nothing
