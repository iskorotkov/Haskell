maxHolidays holidays = foo [1 .. 12] 0 0
  where
    foo []       mv mi = mi
    foo (n : ns) mv mi = if count > mv then foo ns count n else foo ns mv mi
        where count = length (filter (\(d, m) -> m == n) holidays)
