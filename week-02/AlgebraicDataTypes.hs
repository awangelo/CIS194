{-# OPTIONS_GHC -Wall #-}


-- Novo tipo `Coisa` com 4 data constructors (unicos valores do tipo `Coisa`).
data Coisa = Comida
           | Material
           | Ferramenta
           | Roupa
    deriving (Show)
-- `Show` eh uma typeclass (`class Show a where...`), (parece interface).
-- `Coisa` se torna uma instancia de Show pelo `deriving`.

caixaDeCoisas :: [Coisa]
caixaDeCoisas = [Comida, Material, Ferramenta, Roupa]

ehUsadoParaConstruir :: Coisa -> Bool
ehUsadoParaConstruir Material   = True
ehUsadoParaConstruir Ferramenta = True
ehUsadoParaConstruir Roupa      = False
ehUsadoParaConstruir Comida     = False


-- algebraic data types

-- Tipo `FailableDouble` tem dois construtores
data FailableDouble = Failure -- Nao tem argumentos, sozinho eh um valor do tipo `FailableDouble`.
                    | OK Double -- Tem um argumento do tipo Double, por si so nao eh um valor do tipo `FailableDouble`; precisa de um Double.
                                -- `OK` sozinho eh um construtor (funcao).
    deriving Show


-- Data constructors podem ter mais de um argumento
data Pessoa = Pessoa String Int Coisa
    deriving Show
-- Notice how the type constructor and data constructor are both named Person,
-- but they inhabit different namespaces and are different things.
-- This idiom (giving the type and data constructor of a one-constructor type
-- the same name) is common, but can be confusing until you get used to it.

alice :: Pessoa
alice =  Pessoa "Alice" 18 Ferramenta

getAge :: Pessoa -> Int
getAge (Pessoa _ a _ ) = a


-- Algebraic data types, in general
-- In general, an algebraic data type has one or more data constructors, and each data constructor can have zero or more arguments.

-- data AlgDataType = Constr1 Type11 Type12
--                  | Constr2 Type21
--                  | Constr3 Type31 Type32 Type33
--                  | Constr4


-- Pattern-matching

foo :: Pessoa -> String
foo p@(Pessoa n _ _) = "The name field of (" ++ show p ++ ") is " ++ n
-- "The name field of (Pessoa \"Alice\" 18 Ferramenta) is Alice"

check :: Pessoa -> String
check (Pessoa n _ Ferramenta) = n ++ " tem uma ferramenta."
check (Pessoa n _ _)          = n ++ " tem outra coisa."

bar :: [Char] -> [Char]
bar as@(x : xs) = "First element of " ++ show as ++ " is " ++ [x] ++ " folowed by: " ++ xs
bar _ = []


-- Recursive data types

--                [] | n : IntLits
data IntList = Empty | Cons Int IntList

-- Arvore binaria com um bool em cada folha e um int em cada no.
data Tree = Leaf Bool
          | Node Tree Int Tree
    deriving Show

tree :: Tree
tree = Node (Leaf True) 1 (Node (Leaf False) 2 (Leaf False))

--           1
--         /   \
--       T     2
--            / \
--           F   F
