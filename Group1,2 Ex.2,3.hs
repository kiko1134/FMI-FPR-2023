module Main (main) where
  
myGCD :: Int -> Int -> Int
myGCD x y
  | x == 0 = y
  | y == 0 = x
  | otherwise = myGCD y (mod x y)

countDigits :: Int -> Int
countDigits x
  | x == 0 = 0
  | otherwise = 1 + countDigits (div x 10)

sumDigits :: Int -> Int
sumDigits x
  | x == 0 = 0
  | otherwise = mod x 10 + sumDigits (div x 10)

sumDigitsIter :: Integer -> Integer
sumDigitsIter n = sumIter n 0
  where
    sumIter :: Integer -> Integer -> Integer
    sumIter 0 acc = acc
    sumIter n acc = sumIter (n `div` 10) (acc + (n `mod` 10))

reverseNumber :: Integer -> Integer
reverseNumber n = reverseIter n 0
  where
    reverseIter :: Integer -> Integer -> Integer
    reverseIter 0 acc = acc
    reverseIter n acc = reverseIter (n `div` 10) (acc * 10 + (n `mod` 10))

myPow :: Int -> Int -> Int
myPow a b
  | b /= 0 = a * myPow a (b -1)
  | otherwise = 1

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = helper 2
  where
    helper d
      | d == n = True
      | mod n d == 0 = False
      | otherwise = helper (d + 1)

isAscending :: Int -> Bool
isAscending n =
  (n < 10) || ((n `div` 10) `mod` 10 <= n `mod` 10 && isAscending (n `div` 10))

findOccurrences :: Integer -> Integer -> Integer
findOccurrences n d = occurrencesIter n d 0
  where
    occurrencesIter :: Integer -> Integer -> Integer -> Integer
    occurrencesIter 0 _ acc = acc
    occurrencesIter n d acc = occurrencesIter (n `div` 10) d (acc + if n `mod` 10 == d then 1 else 0)

divisors :: Int -> [Int]
divisors n = [x | x <- [1..n], n `mod` x == 0]

isPerfectNumber :: Int -> Bool
isPerfectNumber n = sum (divisors n) == n

primeDivisors :: Int -> [Int]
primeDivisors n = [x| x<-divisors n, isPrime x]

sumPrimeDivisors :: Int -> Int
sumPrimeDivisors n = sum(primeDivisors n)


main :: IO ()
main = do
  print $ myGCD 4 18
  print $ countDigits 12345
  print $ sumDigits 1234
  print $ sumDigitsIter 123
  print $ reverseNumber 123
  print $ myPow 2 4
  print $ isPrime 37
  print $ isAscending 123
  print $ findOccurrences 13233 3
  print $ isPerfectNumber 6
  print $ isPerfectNumber 15
  print $ sumPrimeDivisors 28
  print $ sumPrimeDivisors 15
  print $ sumPrimeDivisors 5