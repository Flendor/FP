pulamea :: [a]->[a]->[a]
pulamea [] [] = []
pulamea [] (hd:tl) = (hd:tl)
pulamea (hd:tl) [] = (hd:tl)
pulamea (hd1:tl1) (hd2:tl2) = (hd1:(pulamea tl1 (hd2:tl2)))