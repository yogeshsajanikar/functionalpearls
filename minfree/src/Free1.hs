
input :: [Int]
input = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]


minfree :: [Int] -> Int
minfree xs = head ( [0..]  \\ xs )

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (not . flip elem vs) us



