module Golf where

import Data.List (intercalate)

skips' :: [a] -> Int -> [a]
skips' [] _ = []
skips' as i = take 1 (drop (i-1) as) ++ skips' (drop i as) i

skips :: [a] -> [[a]]
skips as = map (skips' as) [1..length as]

-- ##########

-- This feels a bit hacky, but I wanted to try it using a fold and this was my first attempt

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c

localMaxima :: [Integer] -> [Integer]
localMaxima = thd3 . foldl isMaxima (Nothing, Nothing, [])

isMaxima :: (Maybe Integer, Maybe Integer, [Integer]) -> Integer -> (Maybe Integer, Maybe Integer, [Integer])
isMaxima (Just x1, Just x2, ms) x3 = 
    if (x1 < x2) && (x2 > x3)
        then (Just x2, Just x3, ms ++ [x2]) 
        else (Just x2, Just x3, ms)
isMaxima (Nothing, Nothing, ms) x1 = (Nothing, Just x1, ms)
isMaxima (Nothing, Just x1, ms) x2 = (Just x1, Just x2, ms)

-- ##########

histLine :: [Integer] -> Integer -> String
histLine xs x = map (\y -> if y >= x then '*' else ' ') xs

histString :: [Integer] -> String
histString xs = intercalate "\n" (histLines ++ ["==========", "0123456789"])
    where histLines = let maxCount = (maximum xs) in
                     map (histLine xs) [maxCount, (maxCount-1)..1]

adjustCounter :: [Integer] -> Integer -> [Integer]
adjustCounter xs x = let x' = (fromInteger x) in
            (fst (splitAt x' xs)) ++ 
            [((head (snd (splitAt x' xs ))) + 1)] ++ 
            (drop 1 (snd (splitAt x' xs)))

histogram :: [Integer] -> String
histogram xs = histString (foldl adjustCounter (take 10 (repeat 0)) xs)
