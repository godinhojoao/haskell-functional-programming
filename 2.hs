-- 1 Questionário
-- 1. Escreva a fun¸c˜ao osQuatroSaoIguais que possui tipo
-- Int -> Int -> Int -> Int -> Bool
-- que retorna True se seus quatro argumentos s˜ao iguais

osQuatroSaoIguais :: Int -> Int -> Int -> Int -> Bool
osQuatroSaoIguais x y c d = (x == y) && (x == c) && (x == d)

-- 2. Defina a fun¸c˜ao quantosSaoIguais :: Int -> Int -> Int -> Int que
-- recebe 3 valores e diz quantos desses valores s˜ao iguais

quantosSaoIguais :: Int -> Int -> Int -> Int
quantosSaoIguais a b c
  | (a == b) && (a == c) && (b == c) = 3
  | (a == b) || (a == c) || (b == c) = 2
  | otherwise = 0

-- 3. Defina a fun¸c˜ao
-- todosDiferentes :: Int -> Int -> Int -> Bool
-- que retorna True se todos os seus argumentos s˜ao diferentes. Obs: m /= n
-- retorna True se m e n s˜ao diferentes
todosDiferentes :: Int -> Int -> Int -> Bool
todosDiferentes a b c = (quantosSaoIguais a b c) == 0
