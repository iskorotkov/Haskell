main =
  print $ nearest [("a", "b", 5), ("a", "c", 4), ("b", "c", 3), ("x", "y", 3)]

nearest []                          = []
nearest cities@((c1, c2, dist) : t) = nearest' cities [(c1, c2)] dist
 where
  nearest' [] results dist = results
  nearest' ((city1, city2, new_dist) : other_cities) results dist
    | new_dist < dist  = nearest' other_cities [(city1, city2)] new_dist
    | new_dist == dist = nearest' other_cities ((city1, city2) : results) dist
    | otherwise        = nearest' other_cities results dist
