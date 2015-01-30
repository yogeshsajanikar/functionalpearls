import Data.List hiding((\\))
import Data.Array


input :: [Int]
input = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]

-- Naive Solution
minfreeNaive :: [Int] -> Int
minfreeNaive xs = head ( [0..]  \\ xs )

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (not . flip elem vs) us

-- Array based Solution
minfreeArray :: [Int] -> Int
minfreeArray = search . checklist

checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False range falseList
  where
    n = length xs
    range = (0, n)
    filterN = filter (<= n)
    falseList = zip (filterN xs) (repeat True)


search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- Divide and conquer
minfree xs = minfrom 0 xs

minfrom a xs | null xs             = a
             | length us == (b -a) = minfrom b vs
             | otherwise           = minfrom a us
  where
    b = a + 1 + length xs `div` 2
    (us, vs) = partition (< b) xs

           
-- Further optimization 
minfreeOpt xs = minfromOpt 0 (length xs, xs)

minfromOpt a (n, xs) | null xs             = a
                     | m == (b -a)         = minfromOpt b (n -m, vs)
                     | otherwise           = minfromOpt a (m, us)
    where
      b = a + 1 + n `div` 2
      (us, vs) = partition (< b) xs
      m = length us



          
