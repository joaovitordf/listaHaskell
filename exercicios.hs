soma :: Integer -> Integer -> Integer
soma x y = x + y

multiplicacao :: Integer -> Integer -> Integer
multiplicacao x y = x * y

cabeca :: [Integer] -> Integer
cabeca x = head x

resto :: [Integer] -> [Integer]
resto x = tail x

media :: Float -> Float -> Float -> Float -> Float
media a b c d = (((a + b) + c) + d) / 4

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt((a**2+b**2))

exemplo1 :: Integer
exemplo1 = a + b
         where {a = 4;
         b = 3}

calc :: Integer -> Integer -> Integer
calc x y = let r = 3
               s = 6
           in r*x + s*y

fatorial :: Integer -> Integer
fatorial x = if x == 0
             then 1
             else x * fatorial (x - 1) 

volume_paralelepipedo :: Int -> Int -> Int -> Int
volume_paralelepipedo b a l = b * a * l

exemplo2 :: (Integer,Integer) -> Integer
exemplo2 (a,b) = a + b

sem_entrada :: Bool
sem_entrada = let a :: Integer
                  a = 3
                  b :: Integer
                  b = 3
                  c :: Integer
                  c = 5
              in a == b && b == c


formula_heron :: Float -> Float -> Float -> Float
formula_heron a b c = 
    let s = calcula_s a b c
    in sqrt(s * (s - a) * (s - b) * (s - c))

calcula_s :: Float -> Float -> Float -> Float
calcula_s a b c = ((a + b) + c) / 2


distancia_pontos :: (Float, Float) -> (Float, Float) -> Float
distancia_pontos (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

ano_bissexto :: Integer -> Bool
ano_bissexto ano | (ano `mod` 4 == 0) && (ano `mod` 100 /= 0) = True
                 | (ano `mod` 400 == 0) = True
                 | otherwise = False

verifica_data :: Integer -> Integer -> Integer -> Bool
verifica_data dia mes ano 
                 | (dia > 0) && (dia < 32) && 
                 (mes > 0) && (mes > 0) &&
                 (ano > 0) = True
                 | otherwise = False

par::Int->Bool
par numero | numero `mod` 2 == 0 = True
           | otherwise = False

conceito :: Float -> Char
conceito nota | nota >= 4  && nota <= 5.99 = 'D'
              | nota >= 6  && nota <= 7.49 = 'C'
              | nota >= 7.5  && nota <= 8.99 = 'B'
              | nota >= 9 = 'A'
              | otherwise = 'E'

type Nome = String
type Idade = Integer
type Peso = Float
type Esporte = String
type Pessoa = (Nome, Idade, Peso, Esporte)

bancoDeDados :: Int -> Pessoa
bancoDeDados identificador | identificador == 1 = ("Joao",21,60.0,"Boxe")
                            | identificador == 2 = ("Maria",21,60.0,"Corridentificadora")
                            | identificador == 3 = ("Pedro",21,60.0,"--")
                            | otherwise = ("--",0,00.0,"--")

recuperaNome :: Int -> Nome
recuperaNome identificador = let (n,_,_,_) = bancoDeDados identificador in n

recuperaidentificadorade :: Int -> Idade
recuperaidentificadorade identificador = let (_,n,_,_) = bancoDeDados identificador in n

recuperaPeso :: Int -> Peso
recuperaPeso identificador = let (_,_,n,_) = bancoDeDados identificador in n

recuperaEsporte :: Int -> Esporte
recuperaEsporte identificador = let (_,_,_,n) = bancoDeDados identificador in n
              