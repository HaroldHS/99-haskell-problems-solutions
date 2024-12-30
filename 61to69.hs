module SixtyOneToSixtyNine where

import FiftyFourAtoSixty

-- Problem 61A
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x] -- leaf
leaves (Branch x lst rst) = leaves lst ++ leaves rst

-- Problem 61
countLeaves :: Tree a -> Int
countLeaves t = length $ leaves t
