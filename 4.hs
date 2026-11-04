data Estacao = Verao | Inverno | Outono | Primavera
  deriving (Show, Eq)
data Temperatura = Calor | Frio
  deriving (Show, Eq)
tempo :: Estacao -> Temperatura
tempo Verao = Calor
tempo _ = Frio

data Forma = Circulo Float | Retangulo Float Float
  deriving (Eq, Show)

redondo :: Forma -> Bool
redondo (Circulo x) = True
redondo (Retangulo x y) = False

area :: Forma -> Float
area (Circulo r) = pi * r * r
area (Retangulo x y) = x*y

meucirculo :: Forma
meucirculo = Circulo 10.1

meuretang :: Forma
meuretang = Retangulo 10 20

data Arvore = Folha Int | Nodo Int Arvore Arvore
  deriving (Show, Eq)

arv2 = Nodo 7 (Nodo 2 (Folha 4) (Folha 3)) (Nodo 7 (Folha 3) (Folha 2))

somArv :: Arvore -> Int
somArv (Folha x) = x
somArv (Nodo x left right) = x + somArv left + somArv right

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (2*n)
multDoisArvore (Nodo n left right) = Nodo (2*n) (multDoisArvore left) (multDoisArvore right)


-- 1
multArvore :: Int -> Arvore -> Arvore
multArvore a (Folha n) = Folha (a*n)
multArvore a (Nodo n left right) = Nodo (a*n) (multArvore a left) (multArvore a right)

-- 2
contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n left right) = (contaFolhas left) + (contaFolhas right)

-- 3
contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n left right) = 1 + (contaNodos left) + (contaNodos right)

-- 4
quantasVezes :: Int -> Arvore -> Int
quantasVezes a (Folha n) = if (a == n) then 1 else 0
quantasVezes a (Nodo n left right) = (if (a == n) then 1 else 0) + (quantasVezes a left) + (quantasVezes a right)