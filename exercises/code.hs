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
           
data Polynomials a = Const Int | Var String | Sum (Polynomials a) (Polynomials a) | Prod (Polynomials a) (Polynomials a)

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

data Tropicals = Finite Int | Infinite
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
