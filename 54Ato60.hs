module FiftyFourAtoSixty where

import Data.Bool

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

-- Problem 55

-- Problem 56
mirror Empty Empty = True
--mirror (Branch n1 l1 r1) (Branch n2 l2 r2) = (n1 == n2) && mirror l1 l2 && mirror r1 r2
mirror (Branch _ l1 r1) (Branch _ l2 r2) = mirror l1 l2 && mirror r1 r2
mirror _ _ = False

symmetric Empty = True
symmetric (Branch x lst rst) = mirror lst rst

-- Problem 57
--
-- insertBST = recursive BST insertion
insertBST x Empty = leaf x
insertBST x (Branch y lst rst) | x < y  = Branch y (insertBST x lst) rst
                               | x > y  = Branch y lst (insertBST x rst)
                               | x == y = Branch y lst rst

construct [] = Empty
construct t = foldl (\root x -> insertBST x root) Empty t 

-- Problem 58

-- Problem 59

-- Problem 60

