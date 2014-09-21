import Data.List

{- 1 -}
nsin n = (iterate sin 1) !! n


{- 2 -}
todigits = unfoldr (\b -> if b == 0 then Nothing else Just (mod b 10, div b 10))

digitslst n = concatMap (\x -> reverse $ todigits x) [1..n]


{- 3 -}
evens lst = concatMap (\n -> if (mod n 2) == 0 then [n, n] else [n]) lst
 
 
{- 4 -}
-- later


{- 5 -}
findmin = (length $ takeWhile (<0.9999) $ map (\n -> sin $ n*n) [1..]) + 1


{- 6 -}
trlst n = map (\x -> x * (x + 1) `div` 2) [1..n]


{- 7 -}
next line = zipWith (+) ([0] ++ line) (line ++ [0])

newton n = (iterate next [1,1]) !! (n-1)


{- 8 -}

erato n = take n $ filter isPrime [2..]
 where
    isPrime n     = not $ any (isDivisor n) [2..n-1]
    isDivisor n d = (mod n d) == 0