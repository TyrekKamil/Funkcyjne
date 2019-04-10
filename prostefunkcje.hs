toSamo :: t-> t
toSamo x = x


kwadrat x = x*x

pusta x = if null x
          then  True
          else  False

silnia x = if x == 1
           then 1
           else silnia(x-1)*x

fib x = if x == 0 
        then 0
        else
        if x == 1
        then 1
        else fib(x-1)+fib(x-2)

ileRazyPrzez :: Integral a => a -> a -> Int
ileRazyPrzez n k | n < k     = 0
                 | otherwise = 1 + ileRazyPrzez (n `div` k) k    

długość :: Integral b => [a] -> b
długość [] = 0
długość (_:t) = (+) 1 (długość t)

suma :: Num a => [a] -> a
suma []    = 0
suma (h:t) = (+) h (suma t)

maxB :: (Bounded a, Ord a) => [a] -> a
maxB []    = minBound
maxB (h:t) = max h (maxB t)     

następne :: Enum a => [a] -> [a]
następne []     = []
następne (x:xs) = succ x : następne xs

parzyste :: Integral a => [a] -> [Bool]
parzyste []     = []
parzyste x = map (\x -> (x `mod` 2 == 0))x


redukcja :: (a -> b -> b) -> b -> [a] -> b
redukcja f s []    = s
redukcja f s (x:xs) = f x (redukcja f s xs)

suma' :: Num a => [a] -> a
suma' = redukcja (+) 0

dlugosc' = redukcja (const (+ 1)) 0


iloczynLog' = redukcja (&&) True


wszystkieParzyste  x = iloczynLog' (map (\x -> (x `mod` 2 == 0))x)
