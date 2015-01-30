import Data.List

input :: [Int]
input = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]


minfree xs = minfrom 0 xs

minfrom a xs | null xs             = a
             | length us == (b -a) = minfrom b vs
             | otherwise           = minfrom a us
  where
    b = a + 1 + length xs `div` 2
    (us, vs) = partition (< b) xs
