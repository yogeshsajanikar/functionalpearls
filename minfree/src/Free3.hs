
-- (as ++ bs) \\ us = (as \\ us ) ++ (bs \\ us)

-- as \\ (us ++ vs) = as \\ us \\ vs


-- (as \\ bs ) \\ cs = as \\ ( bs \\ cs )


-- Suppose as and vs are disjoint => as \\ vs = as
-- bs and us are also disjoint then

(as ++ bs ) \\ ( us ++ vs )
  = as \\ ( us \\ vs ) ++ bs \\ us \\ vs
  = as \\ us ++ bs \\ vs


-- Now we can choose

as = [ 0 .. (b-1) ]
bs = [ b .. ]

--

us = filter (< b) xs
vs = filter (>= b) xs

-- as vs are disjoint, so are bs and us



minfree = head ( xs ++ ys )


head (xs ++ ys) = if null xs then
                    head ys
                  else
                    head xs

null [0..(b-1)] \\ us == length us == b
