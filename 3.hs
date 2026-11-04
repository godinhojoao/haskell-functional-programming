-- (1) Implementar a função
-- multLista :: Int -> [Int] -> [Int]
-- que recebe um inteiro e uma lista e multiplica todos os elementos dessa lista pelo inteiro
double :: Int -> [Int] -> [Int]
double a [] = []
double a (x:xs) = a*x : double a xs

-- (2) Implemente a função
-- elemento :: Int -> [Int] -> Bool
-- que recebe um inteiro e uma lista, e devolve um booleano dizendo se o inteiro se encontra
-- na lista
find :: Int -> [Int] -> Bool
find a [] = False
find a (x:xs) = a == x || find a xs

-- (3) Implemente a função
-- conta :: Int -> [Int] -> Int
-- que recebe um inteiro e uma lista, e diz quantas vezes o inteiro ocorre dentro da lista
count :: Int -> [Int] -> Int
count a [] = 0
count a (x:xs)
  | a == x = 1 + count a xs
  | otherwise = count a xs

-- (4) Implemente a função:
-- contaMaiores :: Int -> [Int]-> Int
-- que recebe um inteiro e uma lista e conta quantos elementos da lista são maiores que o
-- inteiro passado como argumento
countGreater :: Int -> [Int] -> Int
countGreater a [] = 0
countGreater a (x:xs)
  | x > a = 1 + countGreater a xs
  | otherwise = countGreater a xs

-- (5) Implemente a função
-- maiores :: Int -> [Int] -> [Int]
-- que recebe um inteiro e uma lista e devolve uma lista contendo somente os valores que
-- estavam na lista inicial e que são maiores do que o inteiro passado como argumento
getGreaterList :: Int -> [Int] -> [Int]
getGreaterList a [] = []
getGreaterList a (x:xs)
  | x > a = [x] ++ getGreaterList a xs
  | otherwise = getGreaterList a xs

-- (6) Implementar a função
-- geraLista :: Int -> Int -> [Int]
-- que recebe um inteiro m e um inteiro n e devolve uma lista contendo m vezes n
-- > geraLista 3 7
-- [7, 7, 7]
generateList :: Int -> Int -> [Int]
generateList 0 n = []
generateList m n = n:generateList (m-1) n

-- (7) Implementar a função
-- addFim :: Int -> [Int] -> [Int]
-- que recebe um inteiro, uma lista e adiciona o elemento no fim da lista (sem usar o ++):
-- > addFim 10 [1,2]
-- [1,2,10]
addFim :: Int -> [Int] -> [Int]
addFim n [] = [n]
addFim n (x:xs) = x : addFim n xs

-- (8) Implementar a função
-- join :: [Int] -> [Int] -> [Int]
-- que recebe duas listas e concatena as mesmas gerando uma nova lista:
-- > join [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
join :: [Int] -> [Int] -> [Int]
join [] b = b
join (a:as) b = a : join as b
-- join a b = a ++ b -- usando ++ fica assim

-- (9) Implementar a função inverte
-- inverte :: [Int] -> [Int]
-- que recebe uma lista e devolve a mesma invertida
inverte :: [Int] -> [Int]
inverte [] = []
inverte (x:xs) = join (inverte xs) [x]