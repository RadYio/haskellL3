--a) Ecrivez une fonction let2int :: Char -> Int.

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'A'

--b) Ecrivez une fonction int2let :: Int -> Char.

int2let :: Int -> Char
int2let n = chr (ord 'A' + n)

--c) Ecrivez la fonction shift :: Int -> Char -> Char qui effectue le décalage d’un caractère

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int ( toUpper c ) + n) `mod` 26)
          | isUpper c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

--d) Ecrivez une fonction cypher :: Int -> String -> String avec une liste par compréhension.
-- On modifie pour ajouter isalpha et vérifier si c'est une lettre

cypher :: Int -> String -> String
cypher n s = [shift n x | x <- s, isAlpha x]


table :: [Float]
table = [   9.42, --A
            1.02, --B
            2.26, --C
            3.39, --D
            15.87, --E
            0.95, --F
            1.04, --G
            0.77, --H
            8.41, --I
            0.89, --J
            0.001, --K
            5.34, --L
            3.24, --M
            7.15, --N
            5.14, --O
            2.86, --P
            1.06, --Q
            6.46, --R
            7.9, --S
            7.26, --T
            6.24, --U
            2.15, --V
            0.001, --W
            0.3, --X
            0.24, --Y
            0.32] --Z


--e) Commencez par écrire une fonction percent :: Int -> Int -> Float qui calcul le pourcentage d’un entier par rapport à un autre entier

percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

--f) Ecrivez la fonction count :: Char -> String -> Int qui compte le nombre d’occurrences d’un caractère dans une chaîne

count :: Char -> String -> Int
count c s = length [x | x <- s, x == c]

--g) enfin, écrivez la fonction freqs :: String -> [Float] qui retourne une liste des fréquences d’apparition des lettres d’une chaîne quelconque pour l’alphabet [’A’..’Z’]

freqs :: String -> [Float]
freqs s = [percent (count x s) (length s) | x <- ['A'..'Z']]

--h) Ecrivez la fonction chisqr :: [Float] -> [Float] -> Float qui calcul la valeur khi-deux pour deux séries de fréquence données. 

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

--i) Ecrire une fonction rotate :: Int -> [a] -> [a] de rotation vers la gauche de n positions d’une liste. drop et take sont vos amies.

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

--j) Ecrire une fonction positions :: Eq a => a -> [a] -> [Int] qui retourne une liste des positions d’un élément donné dans une liste donnée.

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (y,i) <- zip xs [0..taille], x == y]
    where taille = length xs - 1

--k) Enfin, complétez la fonction crack :: String -> String qui décrypte une chaîne chiffrée. Cette fonction va chercher la valeur de décalage probable en cherchant la valeur minimale de khi-deux.

crack :: String -> String
crack xs = cypher (-factor) xs
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs