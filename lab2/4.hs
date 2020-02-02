maxHolidays holidays = forEveryMonth [1 .. 12]
  where
    forEveryMonth [] = []
    forEveryMonth (n : ns) =
        length (filter (\(d, m) -> m == n) holidays) : forEveryMonth ns
