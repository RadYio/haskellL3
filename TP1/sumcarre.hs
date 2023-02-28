--fonction sumcarre qui fait la somme des 100 premiers carrés d’entiers. On utilisera la fonction sum :: [a] -> a qui fait la somme des valeurs de la liste.
sumcarre :: Int
sumcarre = sum [x^2 | x <- [1..100]]