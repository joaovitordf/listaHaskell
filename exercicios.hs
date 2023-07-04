import Data.Char

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

bd :: Int -> Pessoa
bd identificador | identificador == 1 = ("Joao",2,60.0,"Boxe")
                            | identificador == 2 = ("Maria",21,60.0,"Corridentificadora")
                            | identificador == 3 = ("Pedro",21,60.0,"--")
                            | otherwise = ("--",0,00.0,"--")

-- recuperaNome (bd 1)
recuperaNome :: Pessoa -> Nome
recuperaNome (x,_,_,_) = x

recuperaIdade :: Pessoa -> Idade
recuperaIdade (_,x,_,_) = x

recuperaPeso :: Pessoa -> Peso
recuperaPeso (_,_,x,_) = x

recuperaEsporte :: Pessoa -> Esporte
recuperaEsporte (_,_,_,x) = x

pessoaMaisNova :: Pessoa -> Pessoa -> Pessoa
pessoaMaisNova x y | i1 < i2 = x
                    | otherwise = y
                    where i1 = recuperaIdade x
                          i2 = recuperaIdade y

soma_where::Int
soma_where = b + c
        where b = 1;
              c = 2;

soma_let::Int
soma_let = let b = 1
               c = 2
               in b + c

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = (fibonacci (n - 2)) + (fibonacci (n - 1))

quantidadeMultiplos7 :: Int -> Int
quantidadeMultiplos7 7 = 1
quantidadeMultiplos7 n | n <= 6 = 0
                        | otherwise = 1 + quantidadeMultiplos7 (n-7)

potencia :: Int -> Int -> Int
potencia n p | p == 0 = 1
             | otherwise = n * potencia n (p-1)

verificaPar :: Int -> Bool
verificaPar n | n == 0 = True
              | otherwise = not(verificaPar((n-1)))

somatorio :: Int -> Int
somatorio n | n == 0 = 0
            | otherwise = n + somatorio((n-1))

produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

quantidade :: [a] -> Int
quantidade [] = 0
quantidade (_:xs) = 1 + quantidade xs

alfabeto :: [Char]
alfabeto = ['a'..'z']

numeros :: [Integer]
numeros = [200,199..0]

inverso :: [a] -> [a]
inverso [] = []
inverso (x:xs) = (inverso xs) ++ [x]

get_n :: Int -> [a] -> [a]
get_n 0 _ = []
get_n _ [] = []
get_n n (x:xs) = x : get_n (n-1) (xs)

del_n :: Int -> [a] -> [a]
del_n 0 l = l
del_n _ [] = []
del_n n (_:xs) = del_n (n-1) (xs)

del_ultimo :: [a] -> [a]
del_ultimo [] = []
del_ultimo (_:[]) = []
del_ultimo (x:xs) = x : del_ultimo xs

myConcat :: [[a]] -> [a]
myConcat xss = [x | xs <- xss, x <- xs]

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

primo :: Int -> Bool
primo n = divisores n == [1,n]

primos :: Int -> [Int]
primos n = [x | x <- [1..n], primo x]

zipar :: [a] -> [b] -> [(a,b)]
zipar (a:as) (b:bs) = (a,b) : zipar as bs
zipar _ _ = []

pares :: [a] -> [(a,a)]
pares xs = zipar xs (tail xs)

crescente :: Ord a => [a] -> Bool
crescente xs = and [x <= y | (x,y) <- pares xs]

indices :: Eq a => a -> [a] -> [Int]
indices x ys = [i | (i,y) <- zipar [0..n] ys, x == y]
    where n = length ys - 1

minusculas :: String -> Int
minusculas txt = length [c | c <- txt,
                        c >= 'a' && c <= 'z']

duasVezes :: (a -> a) -> a -> a
duasVezes f x = f (f x)

dobra :: Int -> Int
dobra x = 2 * x

triplica :: Int -> Int
triplica x = 3 * x

app :: (a->b) -> (a,a) -> (b,b)
app f (x,y) = (f x, f y)

mult :: Int -> Int -> Int
mult x y = x * y

dobrar :: Int -> Int
dobrar = mult 2