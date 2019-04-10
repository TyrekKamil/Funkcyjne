--2.2.1
toSamo :: t-> t
toSamo x = x


--2.2.2
kwadrat x = x*x


--2.2.3
pusta x = if null x
          then  True
          else  False


--3.1..
silnia x = if x == 1
           then 1
           else silnia(x-1)*x


--3.1.2
fib x = if x == 0 
        then 0
        else
        if x == 1
        then 1
        else fib(x-1)+fib(x-2)

--3.1.3
ileRazyPrzez :: Integral a => a -> a -> Int
ileRazyPrzez n k | n < k     = 0
                 | otherwise = 1 + ileRazyPrzez (n `div` k) k    

--4.1.1
długość :: Integral b => [a] -> b
długość [] = 0
długość (_:t) = (+) 1 (długość t)

--4.1.2
suma :: Num a => [a] -> a
suma []    = 0
suma (h:t) = (+) h (suma t)

--4.1.3
sumaLog [] = []
sumaLog (x:[]) = x
sumaLog (h:t) = (||) h (sumaLog t)      
 
--4.1.4
maxB :: (Bounded a, Ord a) => [a] -> a
maxB []    = minBound
maxB (h:t) = max h (maxB t)     

--4.1.5
następne :: Enum a => [a] -> [a]
następne []     = []
następne (x:xs) = succ x : następne xs

--4.1.6
parzyste :: Integral a => [a] -> [Bool]
parzyste []     = []
parzyste x = map (\x -> (x `mod` 2 == 0))x

--5.2
redukcja :: (a -> b -> b) -> b -> [a] -> b
redukcja f s []    = s
redukcja f s (x:xs) = f x (redukcja f s xs)

--5.2.1
dlugosc' = redukcja (const (+ 1)) 0

--5.2.2
suma' :: Num a => [a] -> a
suma' = redukcja (+) 0

--5.2.3
iloczynLog' = redukcja (&&) True
--6.2
wszystkieParzyste  x = iloczynLog' (map (\x -> (x `mod` 2 == 0))x)

--7.1
funkcjaStala x = const x
f = funkcjaStala
