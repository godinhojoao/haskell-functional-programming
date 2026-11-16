
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Div E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E    -- menor ou igual
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | SevenTimes C   ---- Executa o comando C 7 vezes
    | DoWhile C B      ---- Do C While B: executa C enquanto B avalie para verdadeiro
    | DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
    | AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
    | Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y 
    | Unless B C C   ---- Unless B C1 C2: se B avalia para falso, então executa C1, caso contrário, executa C2
    | Assert B C     --- caso B avalie para True então executa C. Caso contrário, não faz nada
   deriving(Eq,Show)                


-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------




ebigStep :: (E,Memoria) -> Int
ebigStep (Var x,s) = procuraVar s x
ebigStep (Num n,s) = n
ebigStep (Soma e1 e2,s)  = ebigStep (e1,s) + ebigStep (e2,s)
ebigStep (Sub e1 e2,s)  = ebigStep (e1,s) - ebigStep (e2,s)
ebigStep (Mult e1 e2,s)  = ebigStep (e1,s) * ebigStep (e2,s)
ebigStep (Div e1 e2, s)
   | ebigStep (e2, s) == 0 = error "nao pode dividir por zero"
   | otherwise = ebigStep (e1, s) `div` ebigStep (e2, s)

bbigStep :: (B,Memoria) -> Bool
bbigStep (TRUE,s)  = True
bbigStep (FALSE,s) = False
bbigStep (Not b,s) 
   | bbigStep (b,s) == True     = False
   | otherwise                  = True 
bbigStep (And b1 b2,s )
   | bbigStep (b1,s) == True = bbigStep (b2,s)
   | bbigStep (b1,s) == False = False
bbigStep (Or b1 b2,s )
   | bbigStep (b1,s) == True = True
   | bbigStep (b1,s) == False = bbigStep (b2,s)
bbigStep (Leq e1 e2,s) = ebigStep(e1, s) <= ebigStep(e2, s)
bbigStep (Igual e1 e2,s) = ebigStep(e1, s) == ebigStep(e2, s)

cbigStep :: (C,Memoria) -> (C,Memoria)
cbigStep (Skip,s) = (Skip,s)
cbigStep (If b c1 c2,s)
   | bbigStep (b,s) == True = cbigStep(c1, s)
   | otherwise = cbigStep(c2, s)
cbigStep (Seq c1 c2, s) = seqAux (cbigStep (c1,s)) c2
cbigStep (Atrib (Var x) e,s) = (Skip, mudaVar s x (ebigStep(e,s)) )
cbigStep (While b1 c,s)
   | bbigStep(b1,s) == False = (Skip, s)
   | otherwise = cbigStep(Seq c (While b1 c), s)

-- SevenTimes C: executa C 7 vezes, ou seja, C;C;C;C;C;C;C
cbigStep (SevenTimes c,s) = cbigStep (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c c))))), s)
-- DoWhile C B: executa C uma vez e depois vira um While B C normal
cbigStep (DoWhile c b,s) = cbigStep (Seq c (While b c), s)
-- DuplaATrib: e1 e e2 são avaliadas na memória original, depois faz v1:=n1 e v2:=n2
cbigStep (DuplaATrib (Var v1) (Var v2) e1 e2,s) = (Skip, mudaVar (mudaVar s v1 (ebigStep(e1,s))) v2 (ebigStep(e2,s)))
-- AtribCond b (Var v1) e1 e2: se b for verdade faz v1:=e1, se for falso faz v1:=e2
cbigStep (AtribCond b (Var v1) e1 e2,s)
   | bbigStep (b,s) == True = cbigStep (Atrib (Var v1) e1, s)
   | otherwise = cbigStep (Atrib (Var v1) e2, s)
-- Swap (Var x) (Var y): troca o conteúdo de x e y
cbigStep (Swap (Var x) (Var y),s) = (Skip, mudaVar (mudaVar s x (procuraVar s y)) y (procuraVar s x))
-- Unless B C1 C2: se B for falso executa C1, senão executa C2
cbigStep (Unless b c1 c2,s)
   | bbigStep (b,s) == False = cbigStep (c1, s)
   | otherwise = cbigStep (c2, s)
-- Assert B C: se B for verdade executa C, senão não faz nada
cbigStep (Assert b c,s)
   | bbigStep (b,s) == True = cbigStep (c, s)
   | otherwise = (Skip, s)

seqAux (Skip, s') c2 = cbigStep (c2,s')


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM TODOS OS COMANDOS NOVOS:
-- * SevenTimes
-- * DoWhile
-- * DuplaAtribuicao
-- * AtribCond
-- * Swap
-- * Unless
-- * Assert
-------------------------------------

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação inicial  fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- *Main> ebigStep (progExp1, exSigma)
-- 13
-- *Main> ebigStep (progExp1, exSigma2)
-- 6

--- Para rodar os próximos programas é necessário primeiro implementar as regras da semântica
---


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

---
-- Exemplos dos comandos novos (todos usam exSigma2 = [("x",3), ("y",0), ("z",0)]):

-- SevenTimes: soma 2 em x sete vezes
-- *Main> cbigStep (exSevenTimes, exSigma2)
-- (Skip,[("x",17),("y",0),("z",0)])
exSevenTimes :: C
exSevenTimes = SevenTimes (Atrib (Var "x") (Soma (Var "x") (Num 2)))

-- DoWhile: y recebe a soma de x até 1 (3+2+1)
-- *Main> cbigStep (exDoWhile, exSigma2)
-- (Skip,[("x",0),("y",6),("z",0)])
exDoWhile :: C
exDoWhile = DoWhile (Seq (Atrib (Var "y") (Soma (Var "y") (Var "x")))
                         (Atrib (Var "x") (Sub (Var "x") (Num 1))))
                    (Not (Igual (Var "x") (Num 0)))

-- DoWhile com condição falsa: o corpo executa uma vez mesmo assim
-- *Main> cbigStep (exDoWhile2, exSigma2)
-- (Skip,[("x",3),("y",0),("z",99)])
exDoWhile2 :: C
exDoWhile2 = DoWhile (Atrib (Var "z") (Num 99)) FALSE

-- DuplaATrib: x := x+1 e y := x, as duas usando o x antigo
-- *Main> cbigStep (exDuplaAtrib, exSigma2)
-- (Skip,[("x",4),("y",3),("z",0)])
exDuplaAtrib :: C
exDuplaAtrib = DuplaATrib (Var "x") (Var "y") (Soma (Var "x") (Num 1)) (Var "x")

-- AtribCond: como x <= 5, z recebe 1 (senão receberia 2)
-- *Main> cbigStep (exAtribCond, exSigma2)
-- (Skip,[("x",3),("y",0),("z",1)])
exAtribCond :: C
exAtribCond = AtribCond (Leq (Var "x") (Num 5)) (Var "z") (Num 1) (Num 2)

-- Swap: troca x e y
-- *Main> cbigStep (exSwap, exSigma2)
-- (Skip,[("x",0),("y",3),("z",0)])
exSwap :: C
exSwap = Swap (Var "x") (Var "y")

-- Unless: se x não for 0, faz y := 9 / x, senão faz y := 0
-- *Main> cbigStep (exUnless, exSigma2)
-- (Skip,[("x",3),("y",3),("z",0)])
exUnless :: C
exUnless = Unless (Igual (Var "x") (Num 0))
                  (Atrib (Var "y") (Div (Num 9) (Var "x")))
                  (Atrib (Var "y") (Num 0))

-- Assert: como 1 <= x, faz z := x*x
-- *Main> cbigStep (exAssert, exSigma2)
-- (Skip,[("x",3),("y",0),("z",9)])
exAssert :: C
exAssert = Assert (Leq (Num 1) (Var "x")) (Atrib (Var "z") (Mult (Var "x") (Var "x")))

-- Assert com condição falsa: não faz nada
-- *Main> cbigStep (exAssert2, exSigma2)
-- (Skip,[("x",3),("y",0),("z",0)])
exAssert2 :: C
exAssert2 = Assert (Leq (Var "x") (Num 0)) (Atrib (Var "z") (Num 99))
