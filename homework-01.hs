reversed :: [Integer] -> [Integer]
reversed [] = []
reversed (x:y) = (reversed y) ++ [x]

toDigits :: Integer -> [Integer]
toDigits = reversed . toDigitsRev 
 
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = x `mod` 10 : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith ($) (cycle [id,(*2)]) . reverse

sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate = (0 ==) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
