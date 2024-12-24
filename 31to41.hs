import Data.Bool

-- Problem 31
isPrime :: Int -> Bool
isPrime n | n == 1                                = False
          | n /= 2 && n `mod` 2 == 0              = False
          | n /= 3 && n `mod` 3 == 0              = False
          | any (\x -> n `mod` x == 0) [2..(n-1)] = False -- Naive Approach
          | otherwise                             = True

-- Problem 32
myGCD :: Int -> Int -> Int
myGCD a b | b == 0    = max a (-a)
          | otherwise = myGCD b (a `mod` b)

-- Problem 33
coprime :: Int -> Int -> Bool
coprime a b | myGCD a b == 1 = True
            | otherwise      = False

-- Problem 34
totient :: Int -> Int
totient n = length $ filter (\x -> coprime x n) [1..(n-1)]

-- Problem 35
--primeFactors :: Int -> [Int]
--primeFactors n = 

-- Problem 36


-- Problem 37


-- Problem 38


-- Problem 39
primesR :: Int -> Int -> [Int]
primesR a b = filter (isPrime) [a..b]

-- Problem 40
--
-- NOTE: In case of number with no goldbach conjucture, just return (0,0)
--
goldbach :: Int -> (Int, Int)
goldbach n = let result = [(x,y) | x <- primesR 2 (n-1), let y=(n-x), isPrime y] in
               if length result >= 1 then
                 head result
               else
                 (0, 0)

-- Problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = filter (\(x,y) -> x /= 0 && y /= 0) $ map (goldbach) $ filter (even) [a..b]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b c = filter (\(x,y) -> x >= c && y >= c) $ map (goldbach) $ filter (even) [a..b]

