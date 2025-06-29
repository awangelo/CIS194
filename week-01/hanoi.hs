{-# OPTIONS_GHC -Wall #-}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x s e t = hanoi (x-1) s t e ++ [(s, e)] ++ hanoi (x-1) t e s

-- hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
--                        TROCA ATE QUE O MAIOR ESTEJA LIVRE               TROCA ATE QUE O MAIOR ESTEJA LIVRE 
-- hanoi 3 "a" "b" "c" == [("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")
--                                                           maior no lugar

hanoi4 :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi4 = undefined
