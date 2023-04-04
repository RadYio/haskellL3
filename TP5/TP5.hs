
import Data.Char
import Data.List 
import System.IO

--Ecrivez une fonction getCh :: IO Char qui retournera le caractère saisi. L’écho doit être arrêté pendant la saisie. Pour cela étudiez puis utilisez la fonction hSetEcho du module System.IO

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    c <- getChar
    hSetEcho stdin True
    return c

--Ecrivez une fonction récursive sgetLine :: IO String qui retournera une chaîne saisie. A l’écran, chaque caractère saisi sera remplacé par le caractère ‘-‘ comme dans l’image suivante :

sgetLine :: IO String
sgetLine = do
    c <- getCh
    if c == '\n' then do
        putChar c
        return []
    else do
        putChar '-'
        cs <- sgetLine
        return (c:cs)
    
--Ecrivez une fonction match :: String -> String -> String qui découvre les caractères d’une proposition qui sont compris dans la chaîne à deviner.
--La première chaîne est une proposition et la seconde chaîne est la chaîne à deviner.

match :: String -> String -> String
match lRC lAT = [if elem c lAT then c else '-' | c <- lRC]

--Ecrivez la fonction récursive play :: String -> IO () qui prend comme paramètre la chaîne à deviner et qui gère les propositions de l’adversaire jusqu’à ce qu’il est trouvé.

play :: String -> IO ()
play lAT = do
    putStr "Choix?: "
    lRC <- getLine
    if lRC == lAT then
        putStrLn "Tu as gagné !"
    else do
        putStrLn (match lAT lRC)
        play lAT


--Ecrivez la fonction hangman :: IO () qui demande le mot secret puis fait jouer l’adversaire.

hangman :: IO ()
hangman = do
    putStrLn "Penses à un mot : "
    lAT <- sgetLine
    play lAT


--Ajoutez la fonction principale main = hangman puis compilez votre programme avec le compilateur ghc et exécutez-le directement dans le terminal

main = hangman


















--a) Ecrivez un type matrice fonctionnelle nommé Matf

type Matf = Int -> Int -> (Bool, Int)

--b) Ecrivez la fonction exemple :: Matf.

exemple :: Matf
exemple i j
    | i >= 1 && i <= 6 && j >= 1 && j <= 5 = (True, 2*i+j)
    | otherwise                           = (False, 0)

--c) Ecrivez la fonction identité4x4 représentant une matrice 4 x 4 avec des 1 sur sa diagonale et des 0 partout ailleurs.

identité4x4 :: Matf
identité4x4 i j | i == j && i<=4 = (True, 1)
                | i<=4 && j<=4   = (True, 0)
                | otherwise = (False, 0)


--Ecrivez la fonction dims :: Matf -> (Int, Int) qui retourne les dimensions de la matrice donnée en paramètre.
--Vous écrirez pour cela une fonction nbLines :: Matf -> Int qui retourne le nombre de ligne d’une matrice et nbCols :: Matf -> Int qui retourne le nombre de colonne d’une matrice.

nbLines :: Matf -> Int
nbLines m = nbLines' m 1
    where
        nbLines' m i
            | fst (m i 1) = nbLines' m (i+1)
            | otherwise   = i-1

--Faire la meme chose avec nbCols

nbCols :: Matf -> Int
nbCols m = nbCols' m 1
    where
        nbCols' m i
            | fst (m 1 i) = nbCols' m (i+1)
            | otherwise   = i-1



dims :: Matf -> (Int, Int)
dims m = (nbLines m, nbCols m)

--Ecrivez une fonction cmpDims :: Matf -> Matf -> Bool qui teste si deux matrices sont de même dimensions.

cmpDims :: Matf -> Matf -> Bool
cmpDims m1 m2 = dims m1 == dims m2

--Ecrivez une fonction add :: Matf -> Matf -> Matf qui « fait la somme » des deux matrices données en paramètre (cf. image ci-dessous). 
--Observez que cette fonction add retourne Matf qui est une fonction. 
--Vérifiez que les deux matrices sont de même dimension sinon levez une exception.

add :: Matf -> Matf -> Matf
add m1 m2
    | cmpDims m1 m2 = \i j -> (fst (m1 i j) && fst (m2 i j), snd (m1 i j) + snd (m2 i j))
    | otherwise     = error "Les matrices n'ont pas les memes dimensions"



--Ecrivez une matrice identite6x5

identité6x5 :: Matf
identité6x5 i j | i == j && i<=6 && j<=5 = (True, 1)
                | i<=6 && j<=5 = (True, 0)
                | otherwise = (False, 0)


