maxHolidays holidays = holidaysPerMonth [1 .. 12] 0 0
 where
  holidaysPerMonth [] mv mi = mi
  holidaysPerMonth (n : ns) mv mi | count > mv = holidaysPerMonth ns count n
                                  | otherwise  = holidaysPerMonth ns mv mi
    where count = length (filter (\(d, m) -> m == n) holidays)


maxHolidaysN holidays = reverse (holidaysPerMonth [1 .. 12] 0 [])
 where
  holidaysPerMonth [] mv mi = mi
  holidaysPerMonth (n : ns) mv mi
    | count > mv  = holidaysPerMonth ns count [n]
    | count == mv = holidaysPerMonth ns count (n : mi)
    | otherwise   = holidaysPerMonth ns mv mi
    where count = length (filter (\(d, m) -> m == n) holidays)
