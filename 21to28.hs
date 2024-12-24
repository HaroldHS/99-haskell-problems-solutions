
-- Problem 21
insertAt :: [Char] -> [Char] -> Int -> [Char]
insertAt e s 0 = e ++ s
insertAt e (x:xs) n = [x] ++ insertAt e xs (n-1)

-- Problem 22
range :: Int -> Int -> [Int]
range start stop | start == stop = [stop]
                 | otherwise = [start] ++ range (start+1) stop 

-- Problem 23


-- Problem 24


-- Problem 25


-- Problem 26


-- Problem 27


-- Problem 28


