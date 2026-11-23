
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
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

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)
-- Sub segue as mesmas 3 regras da Soma: primeiro reduz e1, depois e2, depois subtrai
smallStepE (Sub (Num n1) (Num n2), s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el,sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)               = let (el,sl) = smallStepE (e1,s)
                                         in (Sub el e2,sl)


smallStepB :: (B,Memoria) -> (B, Memoria)
-- NOT3 e NOT2: nega o valor, NOT1: reduz um passo dentro do Not
smallStepB (Not FALSE,s)       = (TRUE, s)
smallStepB (Not TRUE,s)        = (FALSE, s)
smallStepB (Not b,s)           = let (bl,sl) = smallStepB (b,s)
                                 in (Not bl, sl)
-- AND2: True and B vira B, AND3: False and B vira False (curto-circuito), AND1: reduz b1
smallStepB (And TRUE b2,s)     = (b2, s)
smallStepB (And FALSE b2,s)    = (FALSE, s)
smallStepB (And b1 b2,s)       = let (bl,sl) = smallStepB (b1,s)
                                 in (And bl b2, sl)
-- OR2: True or B vira True (curto-circuito), OR3: False or B vira B, OR1: reduz b1
smallStepB (Or TRUE b2,s)      = (TRUE, s)
smallStepB (Or FALSE b2,s)     = (b2, s)
smallStepB (Or b1 b2,s)        = let (bl,sl) = smallStepB (b1,s)
                                 in (Or bl b2, sl)
-- LEQ3: compara os dois números, LEQ2: reduz e2, LEQ1: reduz e1
smallStepB (Leq (Num n1) (Num n2), s) = (if n1 <= n2 then TRUE else FALSE, s)
smallStepB (Leq (Num n) e, s)  = let (el,sl) = smallStepE (e,s)
                                 in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s)      = let (el,sl) = smallStepE (e1,s)
                                 in (Leq el e2, sl)
-- Igual: mesmas regras do Leq, só troca o <= por ==
smallStepB (Igual (Num n1) (Num n2), s) = (if n1 == n2 then TRUE else FALSE, s)
smallStepB (Igual (Num n) e, s) = let (el,sl) = smallStepE (e,s)
                                  in (Igual (Num n) el, sl)
smallStepB (Igual e1 e2, s)     = let (el,sl) = smallStepE (e1,s)
                                  in (Igual el e2, sl)


smallStepC :: (C,Memoria) -> (C,Memoria)
-- IF2 e IF3: escolhe o ramo, IF1: reduz a condição
smallStepC (If TRUE c1 c2,s)   = (c1, s)
smallStepC (If FALSE c1 c2,s)  = (c2, s)
smallStepC (If b c1 c2,s)      = let (bl,sl) = smallStepB (b,s)
                                 in (If bl c1 c2, sl)
-- SEQ2: Skip; C2 vira C2, SEQ1: anda um passo em c1 (que pode mudar a memória)
smallStepC (Seq Skip c2,s)     = (c2, s)
smallStepC (Seq c1 c2,s)       = let (cl,sl) = smallStepC (c1,s)
                                 in (Seq cl c2, sl)
-- ATRIB2: com a expressão já reduzida a número, muda a memória, ATRIB1: reduz a expressão
smallStepC (Atrib (Var x) (Num n),s) = (Skip, mudaVar s x n)
smallStepC (Atrib (Var x) e,s) = let (el,sl) = smallStepE (e,s)
                                 in (Atrib (Var x) el, sl)
-- WHILE: desdobra em um if com o corpo seguido do próprio while
smallStepC (While b c, s)      = (If b (Seq c (While b c)) Skip, s)
-- SevenTimes C: vira C;C;C;C;C;C;C
smallStepC (SevenTimes c,s)    = (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c c))))), s)
-- DoWhile C B: executa C uma vez e depois vira um While B C normal
smallStepC (DoWhile c b,s)     = (Seq c (While b c), s)
-- DuplaATrib: reduz e1 até número, depois e2, e só no fim muda as duas variáveis.
-- Como as duas expressões são reduzidas antes de mexer na memória, as duas usam a memória original
smallStepC (DuplaATrib (Var v1) (Var v2) (Num n1) (Num n2),s) = (Skip, mudaVar (mudaVar s v1 n1) v2 n2)
smallStepC (DuplaATrib (Var v1) (Var v2) (Num n1) e2,s) = let (el,sl) = smallStepE (e2,s)
                                                          in (DuplaATrib (Var v1) (Var v2) (Num n1) el, sl)
smallStepC (DuplaATrib (Var v1) (Var v2) e1 e2,s) = let (el,sl) = smallStepE (e1,s)
                                                    in (DuplaATrib (Var v1) (Var v2) el e2, sl)
-- AtribCond b (Var v1) e1 e2: com b reduzido vira a atribuição certa, senão reduz b
smallStepC (AtribCond TRUE (Var v1) e1 e2,s)  = (Atrib (Var v1) e1, s)
smallStepC (AtribCond FALSE (Var v1) e1 e2,s) = (Atrib (Var v1) e2, s)
smallStepC (AtribCond b (Var v1) e1 e2,s)     = let (bl,sl) = smallStepB (b,s)
                                                in (AtribCond bl (Var v1) e1 e2, sl)
-- Swap (Var x) (Var y): é uma dupla atribuição x,y := y,x (as duas leem a memória original)
smallStepC (Swap (Var x) (Var y),s) = (DuplaATrib (Var x) (Var y) (Var y) (Var x), s)
-- Unless B C1 C2: se B for falso executa C1, senão executa C2, se B não for valor reduz B
smallStepC (Unless FALSE c1 c2,s) = (c1, s)
smallStepC (Unless TRUE c1 c2,s)  = (c2, s)
smallStepC (Unless b c1 c2,s)     = let (bl,sl) = smallStepB (b,s)
                                    in (Unless bl c1 c2, sl)
-- Assert B C: se B for verdade executa C, se for falso vira Skip (não faz nada)
smallStepC (Assert TRUE c,s)   = (c, s)
smallStepC (Assert FALSE c,s)  = (Skip, s)
smallStepC (Assert b c,s)      = let (bl,sl) = smallStepB (b,s)
                                 in (Assert bl c, sl)

----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---
--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM
--- OS COMANDOS NOVOS PRINCIPALMENTE O TRATAMENTO DE EXCEÇÕES
--

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])

-- Sub:
-- *Main> interpretadorE (Sub (Var "x") (Num 1), exSigma2)
-- (Num 2,[("x",3),("y",0),("z",0)])


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))

-- *Main> smallStepB (teste1, exSigma2)
-- (Leq (Num 6) (Mult (Num 2) (Num 3)),[("x",3),("y",0),("z",0)])
-- *Main> interpretadorB (teste1, exSigma2)
-- (TRUE,[("x",3),("y",0),("z",0)])
-- *Main> interpretadorB (teste2, exSigma)
-- (FALSE,[("x",10),("temp",0),("y",0)])

-- And, Or, Not e Igual juntos: not (x == 3) or (x <= 5 and true)
-- *Main> interpretadorB (teste3, exSigma2)
-- (TRUE,[("x",3),("y",0),("z",0)])
teste3 :: B
teste3 = Or (Not (Igual (Var "x") (Num 3))) (And (Leq (Var "x") (Num 5)) TRUE)


---
-- Exemplos de Programas Imperativos:

-- *Main> interpretadorC (testec1, exSigma2)
-- (Skip,[("x",0),("y",3),("z",3)])
testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
               (Atrib (Var "y") (Var "z")))

-- *Main> interpretadorC (fatorial, exSigma2)
-- (Skip,[("x",1),("y",6),("z",0)])
fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))

---
-- Exemplos dos comandos novos (todos usam exSigma2 = [("x",3), ("y",0), ("z",0)]):

-- SevenTimes: soma 2 em x sete vezes
-- *Main> interpretadorC (exSevenTimes, exSigma2)
-- (Skip,[("x",17),("y",0),("z",0)])
exSevenTimes :: C
exSevenTimes = SevenTimes (Atrib (Var "x") (Soma (Var "x") (Num 2)))

-- DoWhile: y recebe a soma de x até 1 (3+2+1)
-- *Main> interpretadorC (exDoWhile, exSigma2)
-- (Skip,[("x",0),("y",6),("z",0)])
exDoWhile :: C
exDoWhile = DoWhile (Seq (Atrib (Var "y") (Soma (Var "y") (Var "x")))
                         (Atrib (Var "x") (Sub (Var "x") (Num 1))))
                    (Not (Igual (Var "x") (Num 0)))

-- DoWhile com condição falsa: o corpo executa uma vez mesmo assim
-- *Main> interpretadorC (exDoWhile2, exSigma2)
-- (Skip,[("x",3),("y",0),("z",99)])
exDoWhile2 :: C
exDoWhile2 = DoWhile (Atrib (Var "z") (Num 99)) FALSE

-- DuplaATrib: x := x+1 e y := x, as duas usando o x antigo
-- *Main> interpretadorC (exDuplaAtrib, exSigma2)
-- (Skip,[("x",4),("y",3),("z",0)])
exDuplaAtrib :: C
exDuplaAtrib = DuplaATrib (Var "x") (Var "y") (Soma (Var "x") (Num 1)) (Var "x")

-- AtribCond: como x <= 5, z recebe 1 (senão receberia 2)
-- *Main> interpretadorC (exAtribCond, exSigma2)
-- (Skip,[("x",3),("y",0),("z",1)])
exAtribCond :: C
exAtribCond = AtribCond (Leq (Var "x") (Num 5)) (Var "z") (Num 1) (Num 2)

-- AtribCond com condição falsa: como x == 0 é falso, z recebe 2
-- *Main> interpretadorC (exAtribCond2, exSigma2)
-- (Skip,[("x",3),("y",0),("z",2)])
exAtribCond2 :: C
exAtribCond2 = AtribCond (Igual (Var "x") (Num 0)) (Var "z") (Num 1) (Num 2)

-- Swap: troca x e y
-- *Main> smallStepC (exSwap, exSigma2)
-- (DuplaATrib (Var "x") (Var "y") (Var "y") (Var "x"),[("x",3),("y",0),("z",0)])
-- *Main> interpretadorC (exSwap, exSigma2)
-- (Skip,[("x",0),("y",3),("z",0)])
exSwap :: C
exSwap = Swap (Var "x") (Var "y")

-- Unless: se x não for 0, faz y := x * x, senão faz y := 0
-- *Main> interpretadorC (exUnless, exSigma2)
-- (Skip,[("x",3),("y",9),("z",0)])
exUnless :: C
exUnless = Unless (Igual (Var "x") (Num 0))
                  (Atrib (Var "y") (Mult (Var "x") (Var "x")))
                  (Atrib (Var "y") (Num 0))

-- Unless com condição verdadeira: executa o segundo comando
-- *Main> interpretadorC (exUnless2, exSigma2)
-- (Skip,[("x",3),("y",0),("z",7)])
exUnless2 :: C
exUnless2 = Unless (Leq (Var "y") (Var "x"))
                   (Atrib (Var "z") (Num 1))
                   (Atrib (Var "z") (Num 7))

-- Assert: como 1 <= x, faz z := x*x
-- *Main> interpretadorC (exAssert, exSigma2)
-- (Skip,[("x",3),("y",0),("z",9)])
exAssert :: C
exAssert = Assert (Leq (Num 1) (Var "x")) (Atrib (Var "z") (Mult (Var "x") (Var "x")))

-- Assert com condição falsa: não faz nada
-- *Main> interpretadorC (exAssert2, exSigma2)
-- (Skip,[("x",3),("y",0),("z",0)])
exAssert2 :: C
exAssert2 = Assert (Leq (Var "x") (Num 0)) (Atrib (Var "z") (Num 99))

-- Programa usando vários comandos novos juntos:
-- troca x e y, depois, enquanto y > 0, soma y em z e decrementa y (do while),
-- e por fim, se z for 6, x recebe 100
-- *Main> interpretadorC (exTodos, exSigma2)
-- (Skip,[("x",100),("y",0),("z",6)])
exTodos :: C
exTodos = Seq (Swap (Var "x") (Var "y"))
              (Seq (DoWhile (DuplaATrib (Var "z") (Var "y") (Soma (Var "z") (Var "y")) (Sub (Var "y") (Num 1)))
                            (Not (Leq (Var "y") (Num 0))))
                   (Assert (Igual (Var "z") (Num 6)) (Atrib (Var "x") (Num 100))))
