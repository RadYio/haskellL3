--a) Les opérations doivent être affichées. Ecrivez l’instance Show Op

data Op = Add | Sub | Mul | Div 

instance Show Op where
    show Add = " + "
    show Sub = " - "
    show Mul = " * "
    show Div = " / "

--b) Ecrivez une fonction valid :: Op -> Int -> Int -> Bool qui teste si l’opération donnée appliquée à
--deux entiers est valide pour le jeu.

valid :: Op -> Int -> Int -> Bool
valid Add x y = True
valid Sub x y = x >= y
valid Mul x y = True
valid Div x y = y > 0 && x `mod` y == 0


--c) Ecrivez une fonction apply :: Op -> Int -> Int -> Int qui applique l’opération sur les deux
--nombres donnés. On suppose que l’opération sur les deux entiers est valide pour le jeu (pas
--besoin de tester).

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y


data Expr = Val Int | App Op Expr Expr

--d) Ecrivez l’instance Show Expr

instance Show Expr where
    show (Val x) = show x
    show (App op exp1 exp2) = parenthese exp1 ++ show op ++ parenthese exp2
        where
            parenthese (Val x) = show x
            parenthese exp = "(" ++ show exp ++ ")"

--e) Ecrivez une fonction values :: Expr -> [Int] qui retourne la liste des valeurs d’une expression.

values :: Expr -> [Int]
values (Val x) = [x]
values (App op exp1 exp2) = values exp1 ++ values exp2

--f) Ecrivez une fonction récursive eval qui retourne une liste contenant la valeur finale de
--l’expression. Si l’expression est invalide on retourne une liste vide.

eval :: Expr -> [Int]
eval (Val x) = [x]
eval (App op exp1 exp2) = [apply op x y | x <- eval exp1, y <- eval exp2, valid op x y]

--g)Ecrivez la fonction interleave :: a -> [a] -> [[a]] qui retourne toutes les manières d’insérer un
--élément dans une liste. Vous pouvez utiliser les fonctions take et drop ou définir une récursion en
--vous inspirant du schéma de subs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : [y:zs | zs <- interleave x ys]

--h) Ecrivez la fonction perms qui retourne toutes les permutations possible d’une liste en
--complétant le code suivant

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat [interleave x ys | ys <- perms xs]

--i) Etudiez la fonction subs suivante. Que fait cette fonction ?

--La Fonction subs est recursive et retourne toutes les sous-listes d'une liste donnée 
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = map (x:) (subs xs) ++ subs xs

--j)Ecrivez une fonction choices :: [a] -> [[a]] qui retourne tous les constructions possible à partir
--d’une liste. Cette fonction peut être définie comme les permutations des sous-listes.

choices :: [a] -> [[a]]
choices xs = [zs | ys <- subs xs, zs <- perms ys]

--k) Ecrivez la fonction solution :: Expr -> [Int] -> Int -> Bool . Il faut s’assurer que l’expression
--donnée contient un sous-ensemble de la séquence de nombres donnée et que l’évaluation égale
--la cible. Utilisez la fonction elem :: a -> [a] -> Bool qui teste si un élément fait partie ou non d’une
--liste.

solution :: Expr -> [Int] -> Int -> Bool
solution exp ns n = elem (values exp) (choices ns) && eval exp == [n]

--l)Commencez par écrire une fonction split :: [a] -> [([a],[a])] qui génère toutes les façons possibles
--de découper une liste en deux listes non vides. Vous pouvez appliquer les fonctions take et drop
--dans une liste par compréhension ou définir une récursion.

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

--m)Ecrivez la fonction combine :: Expr -> Expr -> [Expr] qui génère toutes les expressions
--possibles combinant les deux expressions données.

combine :: Expr -> Expr -> [Expr]
combine exp1 exp2 = [App op exp1 exp2 | op <- [Add, Sub, Mul, Div]]

--n) Ecrivez la fonction exprs :: [Int] -> [Expr] qui génère toutes les expressions arithmétiques
--possibles dans le contexte d’une séquence de nombre donnée.

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [ exp | (ls, rs) <- split ns,
                    exp1 <- exprs ls,
                    exp2 <- exprs rs, 
                    exp <- combine exp1 exp2]


--o) Enfin, écrivez la fonction solutions :: [Int] -> Int -> [Expr] qui retourne la liste de toutes les
--expressions valides qui égalent le nombre cible. Utilisez choices pour générer tous les sous-
--ensembles possibles de séquence de nombres, exprs pour générer toutes les expressions sur les
--séquences possibles et eval pour vérifier si chaque expression égale le nombre

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [exp | ns' <- choices ns, exp <- exprs ns', eval exp == [n]]


printSol [] = return ()
printSol (x:xs) = do print x
                     printSol xs

main = printSol $ solutions [1,3,7,10,25,50] 765