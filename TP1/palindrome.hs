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
