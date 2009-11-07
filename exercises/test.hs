module Testing where

import Test.QuickCheck
import Test.HUnit

s = foldTree (\x y z -> x + y + z) (\x y z -> 1 + (if x then 2*y else -3*y) + z) 0 t0
test1 = TestCase (assertEqual "foldTree" 2 s)
