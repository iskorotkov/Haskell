main = print $ 1

winner participants = winner' [] participants 1
 where
  winner' [x]  []       _   = x
  winner' []   [x]      _   = x
  winner' prev []       cnt = winner' [] prev cnt
  winner' prev (x : xs) 5   = winner' prev xs 1
  winner' prev (x : xs) cnt = winner' (prev ++ [x]) xs (cnt + 1)
