import Data.Bool


-- Problem 46, 47, 48
not' :: Bool -> Bool
not' True  = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True = True
and' False  _  = False
and' _   False = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' True    _   = True
or'   _   True  = True

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor'   _    _   = False

eq' :: Bool -> Bool -> Bool
eq' True True   = True
eq' False False = True
eq'   _     _   = False

nand' :: Bool -> Bool -> Bool
nand' = \p q -> not' $ and' p q

nor' :: Bool -> Bool -> Bool
nor' = \p q -> not' $ or' p q

impl' :: Bool -> Bool -> Bool
impl' = \p q -> or' (not' p) q

table :: (Bool -> Bool -> Bool) -> IO()
table f = mapM_ (putStrLn) [ show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [True, False], b <- [True, False]]

-- Problem 49
gray :: Int -> [String]
gray 0 = [""]
gray n = ['0' : x | x <- gray (n-1)] ++ ['1' : x | x <- reverse $ gray (n-1)]

-- Problem 50


