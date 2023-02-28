--fonction qui prend en parametres 4 entiers et qui verifie si ils sont egaux
--si oui, retourne True, sinon False
egaux4 :: Int -> Int -> Int -> Int -> Bool
egaux4 a b c d = if a == b && b == c && c == d then True else False