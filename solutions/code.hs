module Sheets where

import Char (ord)
import List (tails, inits)

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

data Polynomial a = Const a | Var String | 
                    Sum (Polynomial a) (Polynomial a) | 
                    Prod (Polynomial a) (Polynomial a)

instance (Show a) => Show (Polynomial a) where
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

data Tropical = Finite Int | Infinity
instance Show Tropical where
    show (Finite x) = show x
    show Infinity = "Infinity"

instance Eq Tropical
    where
      (Finite x) == (Finite y) = x == y
      Infinity == Infinity = True
      _ == _ = False
  
instance Ord Tropical
    where
      (Finite x) > (Finite y) = x > y
      Infinity > (Finite _) = True
      (Finite _) > Infinity = False

instance PlusTimes Tropical where
    plus (Finite x) Infinity = Finite x
    plus Infinity (Finite x) = Finite x
    plus (Finite x) (Finite y) = Finite $ min x y
    times (Finite x) Infinity = Infinity
    times Infinity (Finite x) = Infinity
    times (Finite x) (Finite y) = Finite $ x + y

class (Ord a, PlusTimes a) => Interpretable a where
    interpret :: Polynomial a -> (String -> a) -> a
    gt :: Polynomial a -> Polynomial a -> (String -> a) -> Bool
    interpret (Var x) m = m x
    interpret (Sum x y) m = plus (interpret x m) (interpret y m)
    interpret (Prod x y) m = times (interpret x m) (interpret y m)
    interpret (Const x) _ = x
    gt polX polY m = (interpret polX m) > (interpret polY m)

-- nothing else needed, the implementation in the class always works
instance Interpretable Int
instance Interpretable Tropical
    
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


-- haming list
--haming = 1 : zipWith (\x -> x : [x * k | k <- [2,3,5]]) haming (tail haming)

mer :: Ord a => [a] -> [a] -> [a]
mer f@(x:xs) s@(y:ys)
    | x < y = x : mer xs s
    | x == y = x : mer xs ys
    | otherwise = y : mer f ys
    
hamming = 1 : mer (map (2*) hamming)
          (mer (map (3*) hamming)
               (map (5*) hamming))

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

coprime :: Int -> [Int]
coprime x = [ y | y <- [1..x], gcd x y == 1 ]

splits :: [a] -> [([a], [a])]
splits xs = [ (take n xs, drop n xs) | n <- [0..(length xs)]]

-- rewrite the binomial product in a constructive way
-- find a function that given a list returns the list with interleaved the sums

-- addSums :: [[Int]] -> [[Int]]
-- addSums l = l ++ [ a + b | (a, b) <- getCouples (last l) ]
--;;--getCouples :: [Int] -> [(Int)]
--build :: Int -> [[Int]]
addSums [] = []
addSums l = newRow (last l) : l
-- newRow :: [Int] -> [(Int, Int)]
newRow :: [Int] -> [Int]
newRow l = head l : [ a + b | (a, b) <- zip (take ((length l) - 1) l) (drop 1 l) ] ++ [last l]

-- we take the last value from the list of the sums of
-- all the possible initial lists
sumWhile :: Int -> [Int] -> Int
sumWhile max xs = last $ takeWhile (< max) (map sum (myInits xs))

myInits                   :: [a] -> [[a]]
myInits []                =  [[]]
myInits (x:xs)            =  [] : map (x:) (myInits xs)


zipWithPad :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithPad f v w [] []         = []
zipWithPad f v w (x:xs) []     = (f x w) : zipWithPad f v w xs []
zipWithPad f v w [] (y:ys)     = (f v y) : zipWithPad f v w [] ys
zipWithPad f v w (x:xs) (y:ys) = (f x y) : zipWithPad f v w xs ys
                                 
nextRow :: [[Integer]] -> [[Integer]]
nextRow (x:xs) = ([1] ++ zipWithPad (+) 0 0 x (drop 1 x)) : nextRow xs
  
pascal2 :: [[Integer]]
pascal2 = [1] : nextRow pascal2


getText :: IO String
getText = do
  x <- getChar
  if x == '\n'
     then
         return ""
     else do
         xs <- getText
         return (x:xs)


getInt :: IO Int
getInt = do
  n <- getText
  return (read n::Int)
  
draw :: IO ()
draw = do
  n <- getInt
  mapM_ (\_ -> putStrLn (concat $ replicate n "*")) [1..n]

data MyMaybe a = Value a | Error

instance Monad MyMaybe where
    return = Value
    Error >>= q = Error
    Value x >>= q = q x
    
instance Show a => Show (MyMaybe a) where
    show Error = "an error occurred"
    show (Value x) = show x
    
data Term = Con Float | Div Term Term | Log Term

eval :: Term -> MyMaybe Float
eval (Log x) = do
  y <- eval x
  if y <= 0 then Error else return $ log y

eval (Con x) = Value x
eval (Div t u) = do
  x <- eval t
  y <- eval u
  if y /= 0 then return (x/y) else Error

-- extend to make possible use of logarithms

data MyMaybe2 a = Value2 a | Error2 String

instance Monad MyMaybe2 where
    return = Value2
    Error2 x >>= q = Error2 x
    Value2 x >>= q = q x
    
instance Show a => Show (MyMaybe2 a) where
    show (Value2 x) = show x
    show (Error2 err) = err

eval2 :: Term -> MyMaybe2 Float
eval2 (Log x) = do
  y <- eval2 x
  if y <= 0
   then Error2 ("Cannot take logarithm of non-positive number " ++ show y ++ "!")
   else return $ log y

eval2 (Con x) = Value2 x
eval2 (Div t u) = do
  x <- eval2 t
  y <- eval2 u
  if y /= 0
   then return (x/y)
   else Error2 "Cannot divide by zero!"


fact :: Int -> Int
fact = \x -> if x <= 0 then 1 else fact (x - 1) * x

four :: Int -> Int
four = \x -> 4
        
f_fact = \g -> \x -> if x <= 0 then 1 else g (x-1) * x

f_four :: (Int -> Int) -> Int -> Int
f_four = \g -> \x -> 4

f_inf  = \g -> \x -> g (x + 4)
f_times = \g -> \(x, y) -> if x <= 0
          then 0
          else y + g(x - 1, y)
               
