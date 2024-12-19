import Data.List

-- Problem 11
data EncodeType = Multiple Int Char | Single Char deriving (Show, Eq)
encodeModified :: [Char] -> [EncodeType]
encodeModified l = map (\l -> (if (length l) > 1 then 
                                   Multiple (length l) (head l)
                               else Single (head l)
                               )) $ group l

-- Problem 12
decodeModified :: [EncodeType] -> [Char]
decodeModified [] = ""
decodeModified (e:es) = case e of Multiple x y -> replicate x y ++ decodeModified es
                                  Single y     -> [y] ++ decodeModified es

-- Problem 13


-- Problem 14
dupli :: [Int] -> [Int]
dupli [] = []
dupli (x:xs) = replicate x x ++ dupli xs

-- Problem 15
repli :: [Char] -> Int -> [Char]
repli "" _ = ""
repli (x:xs) n = replicate n x ++ repli xs n

-- Problem 16


-- Problem 17
split :: [Char] -> Int -> ([Char], [Char])
split xs n = (take n xs, drop n xs)

-- Problem 18
slice :: [Char] -> Int -> Int -> [Char]
slice xs n1 n2 | n1>0 = take (n2-n1+1) $ drop (n1-1) xs
               | otherwise = xs

-- Problem 19


-- Problem 20
removeAt :: Int -> [Char] -> (Char, [Char])
removeAt n xs = (xs !! (n-1), take 1 xs ++ drop n xs)
