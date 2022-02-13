module Set where
import Utils(position, contain, unique)

type Set a = [a]


--
declareSet :: Set Int
declareSet = []


--
fullDeclareSet :: [Int] -> Set Int
fullDeclareSet [] = []
fullDeclareSet (v:vs)
    | isSet (v:vs) == False = arrayToSet (v:vs)
    | otherwise = (v:vs)


--
readSet :: Int -> Set a -> Maybe a
readSet _ [] = error "Index out of bound :: SET"
readSet i (v : vs)
  | i == 0 = Just v
  | i < 0 = error "Index out of bound :: SET"
  | otherwise = Set.readSet (i - 1) vs


--
insertSet :: Int -> Set Int -> Set Int
insertSet elem [] = [elem]
insertSet elem (s : ss) = do
                             case posElem of
                                    -1 -> (s : ss) ++ [elem]
                                    _ -> (s : ss)
                                where
                                     posElem = position elem (s : ss)


--
isSet :: Set Int -> Bool
isSet [] = True
isSet (s : ss) = case has of
                 True -> False
                 False -> isSet ss
                 where
                  has = contain ss s


--
arrayToSet :: [Int] -> [Int]
arrayToSet [] = []
arrayToSet (x:xs) = unique (x:xs)




