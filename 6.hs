-- Para os próximos exercícios, use a seguinte definição de Arvore:
-- data Arvore = Folha Int | Nodo Int Arvore Arvore
-- deriving(Eq,Show)
data Arvore = Folha Int | Nodo Int Arvore Arvore
  deriving (Show, Eq)

arv2 = Nodo 7 (Nodo 2 (Folha 4) (Folha 3)) (Nodo 7 (Folha 3) (Folha 2))
arv3 = Nodo 7 (Nodo 2 (Folha 4) (Folha 3)) (Nodo 112 (Folha 9) (Folha 300))
arv4 = Nodo 7 (Folha 300) (Folha 200)

somArv :: Arvore -> Int
somArv (Folha x) = x
somArv (Nodo x left right) = x + somArv left + somArv right

multDoisArvore :: Arvore -> Arvore
multDoisArvore (Folha n) = Folha (2*n)
multDoisArvore (Nodo n left right) = Nodo (2*n) (multDoisArvore left) (multDoisArvore right)

-- (1) Implementar a função
-- multArvore:: Int -> Arvore -> Arvore
-- que recebe um inteiro e uma árvore, e multiplica todos os valores contidos na árvore pelo
-- inteiro
multArvore :: Int -> Arvore -> Arvore
multArvore a (Folha n) = Folha (a*n)
multArvore a (Nodo n left right) = Nodo (a*n) (multArvore a left) (multArvore a right)

-- (2) Implemente a função
-- contaFolhas :: Arvore -> Int
-- que recebe uma árvore e conta quantas folhas existem nessa árvore
contaFolhas :: Arvore -> Int
contaFolhas (Folha n) = 1
contaFolhas (Nodo n left right) = (contaFolhas left) + (contaFolhas right)

-- (3) Implemente a função
-- contaNodos :: Árvore -> Int
-- que conta quantos Nodos uma árvore possui
contaNodos :: Arvore -> Int
contaNodos (Folha n) = 0
contaNodos (Nodo n left right) = 1 + (contaNodos left) + (contaNodos right)

-- (4) Implemente a função:
-- quantasVezes :: Int -> Arvore -> Int
-- que recebe um inteiro e uma árvore, e conta quantas vezes esse inteiro aparece na árvore
quantasVezes :: Int -> Arvore -> Int
quantasVezes a (Folha n) = if (a == n) then 1 else 0
quantasVezes a (Nodo n left right) = (if (a == n) then 1 else 0) + (quantasVezes a left) + (quantasVezes a right)

-- (5) A função max do Haskell, recebe dois inteiros e devolve o maior entre eles:
-- Main*> max 4 33
-- 33
-- Main*> max 10 10
-- 10
-- Usando a função max implemente a função:
-- maxArvore :: Arvore -> Int
-- que encontra o maior inteiro em uma árvore
maxArvore :: Arvore -> Int
maxArvore (Folha n) = n
maxArvore (Nodo n left right) = max n (max (maxArvore left) (maxArvore right))

-- (6) Uma árvore refletida, é uma árvore com todos os seus ramos esquerdos e direitos trocados.
-- Defina a função:
-- refleteArvore :: Arvore -> Arvore
refleteArvore :: Arvore -> Arvore
refleteArvore (Folha n) = Folha n
refleteArvore (Nodo n left right) = Nodo n (refleteArvore right) (refleteArvore left)

-- (7) Implementar a função
-- geraLista :: Arvore -> [Int]
-- que transforma uma árvore em uma lista de inteiros. Não importa a ordem dos inteiros
-- na lista, apenas que todos os inteiros dos Nodos e Folhas estejam na lista resultante
geraLista :: Arvore -> [Int]
geraLista (Folha n) = [n]
geraLista (Nodo n left right) = n : (geraLista left) ++ (geraLista right)
