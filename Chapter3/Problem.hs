module Chapter3.Problem (Problem (..), Solution (..)) where

data Problem a s = Problem { initial :: s,
                             actions :: s -> [a],
                             result  :: s -> a -> s,
                             goal    :: s -> Bool,
                             cost    :: s -> a -> Double }

data Solution a s = Solution { getActions :: [a],
                               getState   :: s,
                               getCost    :: Double } deriving (Show)
