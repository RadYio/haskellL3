--une fonction pyths qui retourne la liste des triangles rectangles dont l’hypoténuse est de taille n donnée
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,n) | x <- [1..n], y <- [1..n],
                     x^2 + y^2 == n^2]
