findSize:: [a]->Int
findSize [] = 0
findSize(_:x) = 1+findSize x

findSum:: Num a => [a]->a
findSum = sum

isPresent:: Eq a => a->[a]->Bool
isPresent _ [] = False
isPresent e (x:xs)
  |e == x = True
  |otherwise = isPresent e xs

primeNumsInterval:: Int -> Int -> [Int]
primeNumsInterval a b
  |a==b = error "interval numbers cannot be equal"
  |otherwise = [x|x<-[a..b],isPrime x]

removeFirstElement:: Eq a => a->[a] -> [a]
removeFirstElement _ [] = error "The input array is empty"
removeFirstElement z (x:xs) =
  if z == x then xs
  else x:removeFirstElement z xs

removeAllX:: Eq a => a-> [a] -> [a]
removeAllX _ []= error "the input array is empty"
removeAllX z xs = [x | x <-xs, x /=z]

incrementAllBy:: [Int] -> Int -> [Int]
incrementAllBy [] _ = error "the input array is empty"
incrementAllBy arr n = map (+n) arr

incrementAllBy':: [Int] -> Int -> [Int]
incrementAllBy' ns k = [n+k|n<-ns]

main :: IO ()
main = do
    let myList = [1,2,3,4,5,2,6,1]
    print $ findSize myList
    print $ findSum myList
    print $ isPresent 6 myList
    print $ isPresent 7 myList
    print $ primeNumsInterval 1 8
    print $ removeFirstElement 2 [1,2,3,2]
    print $ incrementAllBy'[1,2,3] 3
    print $ incrementAllBy myList 1
