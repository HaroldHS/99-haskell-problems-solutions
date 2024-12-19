import Data.Bool
import Data.List (group)

-- Problem 1
myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt l n = y where [y] = drop (n-1) (take n l)

-- Problem 4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) =  myReverse xs ++ [x]

-- Problem 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x | x == (myReverse x) = True
               | otherwise = False

-- Problem 7


-- Problem 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x] -- To prevent head([]) error
compress (x:xs) | x == head(xs) = compress xs
                | otherwise = [x] ++ compress xs

-- Problem 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [b] = [[b]]
pack (x:xs) = let(a, b) = span (==x) (x:xs) in
                  [a] ++ pack b

-- Problem 10
encode :: (Eq a) => [a] -> [(Int,a)]
encode l = map (\l -> (length l, head l)) $ group l
