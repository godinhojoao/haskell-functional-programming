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