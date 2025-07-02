{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x s e t = hanoi (x-1) s t e ++ [(s, e)] ++ hanoi (x-1) t e s

-- hanoi 2 "a" "b" "c" == [("a", "c"), ("a", "b"), ("c", "b")]
--                        TROCA ATE QUE O MAIOR ESTEJA LIVRE               TROCA ATE QUE O MAIOR ESTEJA LIVRE 
-- hanoi 3 "a" "b" "c" == [("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")
--                                                           maior no lugar

-- https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame%E2%80%93Stewart_algorithm
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 x s e t1 t2 =
  let k = x - round (sqrt (2 * fromIntegral x + 1)) + 1
  in  hanoi4 k s t1 t2 e -- move k discos do topo para uma vareta auxiliar
      ++ hanoi (x - k) s e t2 -- move os discos de baixo (x - k) para o final, sem atrapalhar a vareta com os k discos (usando apenas 3 varetas)
      ++ hanoi4 k t1 e s t2 -- termina movendo os k discos do topo para o final