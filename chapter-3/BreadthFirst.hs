module BreadthFirst (search) where
import Problem (Problem (..), Solution (Solution))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Queue.Class as Q
import Data.Queue.Queue (Queue)

data Node a s = Node { getActions :: [a],
                       getState :: s,
                       getCost :: Double }

movenode :: Problem a s -> Node a s -> a -> Node a s
movenode problem (Node as s c) action = Node (action:as) (result problem s action) (c + cost problem s action)

unvisited :: (Ord s) => Problem a s -> Set s -> s -> [a]
unvisited problem closed state = filter (not . (flip Set.member closed) . (result problem state)) (actions problem state)

search :: (Ord s) => Problem a s -> Maybe (Solution a s)
search problem = search' problem (Q.singleton (Node [] (initial problem) 0)) (Set.empty)

type Frontier a s = Queue (Node a s)
type Closed s = Set s
search' :: (Ord s) => Problem a s -> Frontier a s -> Closed s -> Maybe (Solution a s)
search' problem frontier closed = case Q.extract frontier of
                                    Just (top, frontier') ->
                                        if goal problem (getState top) then
                                            Just $ case top of { Node a s c -> Solution a s c }
                                        else
                                            let closed' = Set.insert (getState top) closed
                                                tovisit = unvisited problem closed' (getState top)
                                                nodes = map (movenode problem top) tovisit
                                                frontier'' = Q.insertAll nodes frontier'
                                            in search' problem frontier'' closed'
                                    Nothing -> Nothing
