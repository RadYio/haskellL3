type Domino = (Int, Int)

--Fonction dominosA2Match
dominosA2Match :: Domino -> Domino -> Bool
dominosA2Match (a,b) (c,d) = a == c || a == d || b == c || b == d

--Fonction dominosA3Match qui teste si trois dominos peuvent être assemblés on pourra crée un dominos virtuel pour tester
dominosA3Match :: Domino -> Domino -> Domino -> Bool
dominosA3Match (a,b) (c,d) (e,f) = (a == c && b == d) || (a == d && b == c) || (a == e && b == f) || (a == f && b == e)