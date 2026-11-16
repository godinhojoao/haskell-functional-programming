data Nat = Zero | Suc Nat
  deriving(Eq,Show)

four = Suc (Suc (Suc (Suc(Zero))))
three = Suc(Suc (Suc(Zero)))
two = Suc (Suc(Zero))

-- implementar as seguintes funções:
-- (1) Implementar a função to_int que converte um Nat para um inteiro:
to_int :: Nat -> Int
to_int Zero = 0
to_int (Suc n) = 1 + (to_int n)

-- (2) Implementar a função to_nat que transforma um inteiro em um Nat:
to_nat :: Int -> Nat
to_nat 0 = Zero
to_nat n = Suc(to_nat (n-1))

-- (3) Implementar usando recursão a função mult que multiplica dois Nats. (Você NÃO pode
-- usar to_int ou to_nat):
soma :: Nat -> Nat -> Nat
soma Zero n = n
soma (Suc n1) n2 = soma n1 (Suc n2)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Suc n1) n2 = soma n2 (mult n1 n2)

-- (4) Implementar a função leq (menor ou igual) que recebe dois Nats e devolve um booleano.
-- (Você NÃO pode usar to_int ou to_nat):
leq :: Nat -> Nat -> Bool
leq Zero n = True
leq n Zero = False
leq (Suc(n1)) (Suc(n2)) = leq n1 n2

-- (5) Implementar a função sub que executa a subtração de dois naturais (Você NÃO pode
-- usar to_int ou to_nat):
sub :: Nat -> Nat -> Nat
sub n Zero = n
sub Zero n = error "Nao existem números negativos nos Naturais"
sub (Suc n1) (Suc n2) = sub n1 n2

-- (6) Implemente recursivamente a função geq (maior ou igual) que verifica se o primeiro
-- argumento é maior ou igual do que o segundo (Você NÃO pode usar to_int ou to_nat):
geq :: Nat -> Nat -> Bool
geq n Zero  = True
geq Zero n = False
geq (Suc(n1)) (Suc(n2)) = geq n1 n2

-- (7) A divisão pode ser calculada através de subtrações sucessivas. Implemente a função divi
-- usando essa técnica (Você NÃO pode usar to_int ou to_nat):
divi :: Nat -> Nat -> Nat
divi Zero n = Zero
divi n Zero = error "Nao pode dividir por 0"
divi (Suc(n1)) n2 = sub n2 (divi n1 n2)

-- (8) Como você modificaria a função anterior para que ela retorne o resto da divisão?
-- resto_div :: Nat -> Nat -> Nat
lessThan :: Nat -> Nat -> Bool
lessThan Zero Zero = False
lessThan Zero n = True
lessThan n Zero = False
lessThan (Suc(n1)) (Suc(n2)) = lessThan n1 n2

resto_div :: Nat -> Nat -> Nat
resto_div n Zero = error "Nao pode dividir por 0"
resto_div Zero n = Zero
resto_div n1 n2
  | lessThan n1 n2 = n1
  | otherwise = resto_div (sub n1 n2) n2
