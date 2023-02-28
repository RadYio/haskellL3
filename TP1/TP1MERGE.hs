-------------------------------
---------- Exercice 2 ---------
-------------------------------

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort ( x : xs ) = qsort smaller ++ [x] ++ qsort larger
 where smaller = [ a | a <- xs, a <= x ]
       larger = [ a | a <- xs, a > x ]
       

-------------------------------
---------- Exercice 3 ---------
-------------------------------

--fonction qui prend en parametres 4 entiers et qui verifie si ils sont egaux
--si oui, retourne True, sinon False
egaux4 :: Int -> Int -> Int -> Int -> Bool
egaux4 a b c d = if a == b && b == c && c == d then True else False

--fonction qui prend en parametres 4 entiers et retourne le maximum
max4 :: Int -> Int -> Int -> Int -> Int
max4 a b c d = max a (max b (max c d))

-------------------------------
---------- Exercice 4 ---------
-------------------------------



--fonction sumcarre qui fait la somme des 100 premiers carrés d’entiers. On utilisera la fonction sum :: [a] -> a qui fait la somme des valeurs de la liste.
sumcarre :: Int
sumcarre = sum [x^2 | x <- [1..100]]

replic :: Int -> a -> [a]
replic 0 _ = []
replic n x = x : replic (n-1) x

--une fonction pyths qui retourne la liste des triangles rectangles dont l’hypoténuse est de taille n donnée
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,n) | x <- [1..n], y <- [1..n],
                     x^2 + y^2 == n^2]


-------------------------------
---------- Exercice 5 ---------
-------------------------------


--Fonction inverse une liste recusivement
inverse :: [a] -> [a]
inverse [] = []
inverse (x:xs) = inverse xs ++ [x]

--Fonction palindrome qui teste si une liste est un palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == inverse xs

--Fonction doPalindrome qui créer un palindrome à partir d’une liste
doPalindrome :: [a] -> [a]
doPalindrome xs = xs ++ inverse (init xs)


-------------------------------
---------- Exercice 6 ---------
-------------------------------


--On crée un type parfum

data Parfum = Chocolat | Vanille | Framboise


prixParfum :: Parfum -> Float
prixParfum Chocolat = 1.5
prixParfum Vanille = 1.2
prixParfum Framboise = 1.4


--V=========================
--Version uniquement 3 boule demandée dans le TP
--V=========================
data Glace3 = Boule Parfum | Double Parfum Parfum | Triple Parfum Parfum Parfum

prixGlace3 :: Glace3 -> Float
prixGlace3 (Boule p) = prixParfum p + 0.1
prixGlace3 (Double p1 p2) = prixParfum p1 + prixParfum p2 + 0.15
prixGlace3 (Triple p1 p2 p3) = prixParfum p1 + prixParfum p2 + prixParfum p3 + 0.2

--V=========================
--Version pour utiliser map
--V=========================

type GlaceM = [Parfum]
prixGlaceM :: GlaceM -> Float
prixGlaceM [] = 0
prixGlaceM glace = sum (map prixParfum glace)

--Si on veut ajouter le prix du cornet
--prixGlaceM glace = sum (map prixParfum glace) + 0.05 + 0.05*(length glace)




-------------------------------
---------- Exercice 7 ---------
-------------------------------


type Domino = (Int, Int)

--Fonction dominosA2Match
dominosA2Match :: Domino -> Domino -> Bool
dominosA2Match (a,b) (c,d) = a == c || a == d || b == c || b == d

--Fonction dominosA3Match qui teste si trois dominos peuvent être assemblés 
dominosA3Match :: Domino -> Domino -> Domino -> Bool
dominosA3Match (a,b) (c,d) (e,f) = (a == c && b == d) || (a == c && b == f) || (a == d && b == c) || (a == d && b == e) || (a == e && b == c) || (a == e && b == f) || (a == f && b == d) || (a == f && b == e)