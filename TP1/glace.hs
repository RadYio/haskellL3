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