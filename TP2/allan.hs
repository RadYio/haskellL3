-------------------------------
---------- Exercice 1 ---------
-------------------------------

--a) Ecrivez les fonctions de test paire et impair qui s’appellent l’une et l’autre. On considère que 0 est paire.

paire :: Int -> Bool
paire x | x == 0 = True
        | otherwise = impaire (x - 1)

impaire :: Int -> Bool
impaire x | x == 0 = False
          | otherwise = paire (x - 1)

--b) Ecrivez une fonction insert :: Ord a => a -> [a] -> [a] d’insertion d’un élément dans une liste déjà ordonnée
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y:insert x ys

--c) Ecrivez la fonction isort :: Ord a => [a] -> [a] de tri d’une liste par insertion. Utilisez insert.

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-------------------------------
---------- Exercice 2 ---------
-------------------------------

--a) Ecrivez la fonction halve :: [a] -> ([a], [a]) qui coupe une liste en deux et retourne un tuple d’arité 
--2. Utilisez take et drop vos deux amies

halve :: [a] -> ([a], [a])
halve x = (take (length x `div` 2) x, drop (length x `div` 2) x)

--b) Ecrivez la fonction merge :: Ord a => [a] -> [a] -> [a] qui fusionne 2 listes triées en une seule liste triée.
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] (y:ys) = y:ys
merge (x:xs) [] = x:xs
merge (x:xs) (y:ys) | x <= y = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys


--c) Ecrivez la fonction msort :: Ord a => [a] -> [a] de tri par fusion. On utilisera merge et halve.
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort list = merge(msort x) (msort y) 
    where (x,y) = halve list


-------------------------------
---------- Exercice 3 ---------
-------------------------------

--Partie 1

data Prop = Const Bool
    | Var Char
    | Not Prop
    | And Prop Prop
    | Imply Prop Prop

p1:: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A')(Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k, v)]           -- Tableau associatif ( clé, valeur )
type Subst = Assoc Char Bool        -- Tableau associatif ( une_variable, sa_valeur )



--a) Ecrivez la fonction find :: Eq k => k -> Assoc k v -> v . Vous pouvez utiliser une liste par compréhension ou définir récursivement la fonction. On considère que k existe forcément dans la liste associative.

find :: Eq k => k -> Assoc k v -> v
find k ((x,y):xs) | k == x = y
                  | otherwise = find k xs

--b) Ecrivez la fonction eval :: Subst -> Prop -> Bool qui évalue une proposition donnée selon une liste de substitution donnée.

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b  
eval s (Var x) = find x s  
eval s (Not p) = not (eval s p)  
eval s (And p q) = (eval s p) && (eval s q)  
eval s (Imply p q) = not (eval s p) || (eval s q)  





