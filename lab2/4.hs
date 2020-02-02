maxHolidays holidays = foo [1 .. 12] 0 0
  where
    foo [] mv mi = mi
    foo (n : ns) mv mi | count > mv = foo ns count n
                       | otherwise  = foo ns mv mi
        where count = length (filter (\(d, m) -> m == n) holidays)


maxHolidaysN holidays = reverse (foo [1 .. 12] 0 [])
  where
    foo [] mv mi = mi
    foo (n : ns) mv mi | count > mv  = foo ns count [n]
                       | count == mv = foo ns count (n : mi)
                       | otherwise   = foo ns mv mi
        where count = length (filter (\(d, m) -> m == n) holidays)
