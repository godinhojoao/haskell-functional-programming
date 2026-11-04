-- Desafios de Listas

-- 1. Implementar a função slice, que devolve um pedaço interno de uma lista:
-- *Main> slice 3 7 [’a’,’b’,’c’,’d’,’e’,’f’,’g’,’h’,’i’,’k’]
-- "cdefg"
slice :: Int -> Int -> []


-- 2. Implementar a função compress que elimina elementos repetidos consecu-
-- tivos
-- > compress "aaaabccaadeeee"
-- "abcade"

-- 3. Implementar a função pack que coloca elementos repetidos consecutivos em
-- sublistas
-- > pack [’a’, ’a’, ’a’, ’a’, ’b’, ’c’, ’c’, ’a’,
-- ’a’, ’d’, ’e’, ’e’, ’e’, ’e’]
-- ["aaaa","b","cc","aa","d","eeee"]
-- > pack "bbaccczwwwq"
-- ["bb","a","ccc","z","www","q"]

-- 4. Implementar a função encode:
-- > encode "aaaabccaadeeee"
-- [(4,’a’),(1,’b’),(2,’c’),(2,’a’),(1,’d’),(4,’e’)]

-- 5. Implementar a função rotate
-- *Main> rotate 3 ""abcdefgh"
-- "defghabc"
-- *Main> rotate (-2) [’a’,’b’,’c’,’d’,’e’,’f’,’g’,’h’]
-- "ghabcdef"