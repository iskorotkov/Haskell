main =
  print $ nearest [("a", "b", 5), ("a", "c", 4), ("b", "c", 3), ("x", "y", 3)]

nearest [] = []
nearest cities = nearest' cities [first_cities_pair cities] (first_dist cities)
  where
    first_cities_pair cities = get_cities (head cities)
      where
        get_cities (a, b, c) = (a, b)

    first_dist cities = dist (head cities)
      where
        dist (a, b, c) = c

    nearest' [] results dist = results
    nearest' ((city1, city2, new_dist):other_cities) results dist
      | new_dist < dist = nearest' other_cities [(city1, city2)] new_dist
      | new_dist == dist = nearest' other_cities ((city1, city2):results) dist
      | otherwise = nearest' other_cities results dist
