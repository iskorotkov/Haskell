main = print $ (conjuction 8 10)

conjuction n m =
    if m > n then 0
    else div (fact n) (fact m * fact (n - m)) where
        fact 0 = 1
        fact n = n * fact (n - 1)
