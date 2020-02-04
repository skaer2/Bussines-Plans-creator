module Calculations where

import           Control.Monad.State.Lazy

-- takes two lists: a list with Incomes, and a list with expenses, then calculates the list of balance on each element of the list accounting for previous elements
calculateBalance :: [Int] -> [Int] -> State Int [Int]
calculateBalance [] _ = return []
calculateBalance _ [] = return []
calculateBalance (x:xs) (y:ys) = do
    funds <- get
    let balance = (x - y) + funds
    return $ balance : (evalState (calculateBalance xs ys) $ funds + balance)
