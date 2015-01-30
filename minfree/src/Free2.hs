import Data.Array


input :: [Int]
input = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]

minfree :: [Int] -> Int
minfree = search . checklist

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False range falseList
  where
    n = length xs
    range = (0, n)
    filterN = filter (<= n)
    falseList = zip (filterN xs) (repeat True)


search :: Array Int Bool -> Int
search = length . takeWhile id . elems


countList :: [Int] -> Array Int Int
countList xs = accumArray (+) 0 range (zip (filter (<= n) xs) (repeat 1))
  where
    n = length xs
    range = (0, n)
