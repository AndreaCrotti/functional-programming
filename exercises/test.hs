module Testing where

import Test.QuickCheck
import Test.HUnit

s = foldTree (\x y z -> x + y + z) (\x y z -> 1 + (if x then 2*y else -3*y) + z) 0 t0
test1 = TestCase (assertEqual "foldTree" 2 s)

pi1,pi2 :: Polynomial Int
pi1 = (Product (Sum (Const 4) (Var "a")) (Var "b"))
pi2 = (Product (Sum (Const 4) (Var "b")) (Var "a"))
valuesi :: String -> Int
valuesi = \x -> case x of "a" -> 7 ; "b" -> 4
pt1,pt2 :: Polynomial Tropical
pt1 = (Product (Sum (Const (Finite 4)) (Var "a")) (Var "b"))
pt2 = (Product (Sum (Const (Finite 4)) (Var "b")) (Var "a"))
valuest :: String -> Tropical
valuest = \x -> case x of "a" -> (Finite 7) ; "b" -> Infinity