module Stack where

type Stack a = [a]

declareStack ::  Stack Int
declareStack = []


pushValue :: Stack Int -> Int -> Stack Int
pushValue [] elem = [elem]
pushValue stack elem = [elem] ++ stack


popValue :: Stack Int -> Stack Int
popValue [] = []
popValue (s:ss) = ss

