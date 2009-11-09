module Sheets where

import Char (ord)

-- code from sheets for easy testing

f :: (Int -> Bool) -> (Bool -> Int)
f fun = (\x -> 10)
g :: (Bool -> Int) -> (Int -> Bool)
g fun = (\x -> True)

h x y = f (g (f y)) x

divisibleByTwo :: Int -> Bool
divisibleByTwo x
               | x < 0 = divisibleByTwo $ abs x
               | x == 0 = True
               | x == 1 = False
               | otherwise = divisibleByTwo (x - 2)

divisibleByTwoList :: [Int] -> Bool
divisibleByTwoList [] = False
divisibleByTwoList (x : xs) =
                             (divisibleByTwo x) || (divisibleByTwoList xs)

power :: Int -> Int -> Int
power x y
    | y < 0 = undefined
    | y == 0 = 1
    | divisibleByTwo y = temp * temp
    | otherwise = x * (power x (y - 1)) 
    where temp = power x (div y 2)

(><) :: [a] -> [b] -> [(a,b)]
infixr 6 ><
xs >< ys = [ (x, y) | x <- xs, y <- ys]
   
-- sumList using only case operator and recursion
sumList :: [Int] -> Int
sumList (x:xs) =
    case xs of
      [] -> 0
      _ -> x + sumList xs
           
-- Sheet 3

data Polynomials a = Const a | Var String | Sum (Polynomials a) (Polynomials a) | Prod (Polynomials a) (Polynomials a)

instance (Show a) => Show (Polynomials a) where
    show (Var x) = x
    show (Const x) = show x
    show (Prod x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (Sum x y) = "(" ++ show x ++ " + " ++ show y ++ ")"

class PlusTimes a where
    plus :: a -> a -> a
    times :: a -> a -> a
               
instance PlusTimes Int where
    plus x y = x + y
    times x y = x * y

data Tropicals = Finite Int | Infinite deriving (Eq, Ord)
instance Show Tropicals where
    show (Finite x) = show x
    show Infinite = "Infinity"

instance PlusTimes Tropicals where
    plus (Finite x) Infinite = Finite x
    plus Infinite (Finite x) = Finite x
    plus (Finite x) (Finite y) = Finite $ min x y
    times (Finite x) Infinite = Infinite
    times Infinite (Finite x) = Infinite
    times (Finite x) (Finite y) = Finite $ x + y

class (Ord a, PlusTimes a) => Interpretable a where
    interpret :: Polynomials a -> (String -> a) -> a
    gt :: Polynomials a -> Polynomials a -> (String -> a) -> Bool
    interpret (Var x) m = m x
    interpret (Sum x y) m = plus (interpret x m) (interpret y m)
    interpret (Prod x y) m = times (interpret x m) (interpret y m)
    interpret (Const x) _ = x
    gt polX polY m = (interpret polX m) > (interpret polY m)

-- nothing else needed, the implementation in the class always works
instance Interpretable Int
instance Interpretable Tropicals
    
strToNum :: String -> Int
strToNum = sum . map ord

mappingTest :: String -> Int
mappingTest x = 
    case x of
      "a" -> 10
      "b" -> 11
      "c" -> 5
      otherwise -> 0

t0 = (Node 1 (Node True (Node 2 Empty Empty) Empty)(Node False (Node 3 Empty Empty) (Node 4 Empty Empty)))

data Tree a b = Empty | Node a (Tree b a) (Tree b a) deriving Show
  
mapTree :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
mapTree _ _ Empty = Empty
mapTree f1 f2 (Node x y z) =
    Node (f1 x) (mapTree f2 f1 y) (mapTree f2 f1 z)

foldTree :: (a -> c -> c -> c) -> (b -> c -> c -> c) -> c -> Tree a b -> c
-- "foldTree fa fb e t" replaces all occurences of
foldTree _ _ e Empty = e
foldTree fa fb e (Node x y z) = fa x (foldTree fb fa e y) (foldTree fb fa e z)

countABs :: Tree a b -> (Int, Int)
countABs t = (foldTree (adder 1) (adder 0) 0 t, foldTree (adder 0) (adder 1) 0 t)
    where adder w = \_ y z -> w + y + z
