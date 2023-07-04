-- Inverso de uma lista

inverte :: [a] -> [a]
inverte [] = []
inverte (x:xs) = (inverte xs) ++ [x]

-- Crie uma função para remover os “n” primeiros elementos de uma lista

del_n :: Int -> [a] -> [a]
del_n 0 l = l
del_n _ [] = []
del_n n (_:xs) = del_n (n-1) (xs)

-- Crie uma função que remove os “n” últimos elementos de uma lista

del_n_ultimos :: Int -> [a] -> [a]
del_n_ultimos _ [] = []
del_n_ultimos 0 l = l
del_n_ultimos n xs = inverte (del_n n (inverte xs))

-- Crie uma função que remove o n-ésimo elemento de uma lista

del_nesimo :: Int -> [a] -> [a] -> [a]
del_nesimo _ [] _ = []
del_nesimo 0 xs ys = tail xs ++ ys
del_nesimo n (x:xs) ys = del_nesimo (n-1) xs (x:ys)

-- Crie uma funcao que pegue os n primeiros elementos

n_primeiros :: Int -> [a] -> [a]
n_primeiros _ [] = []
n_primeiros 0 l = l
n_primeiros n lista = del_n_ultimos (length(lista)-n) lista


-- Crie uma funcao que pegue os n ultimos elementos

n_ultimos :: Int -> [a] -> [a]
n_ultimos _ [] = []
n_ultimos 0 l = l
n_ultimos n lista = inverte (n_primeiros n (inverte (lista)))

-- exercicio de escolher uma letra e separar o que vem antes e depois da letra na string

separaLetra :: String -> Char -> [String]
separaLetra [] _ = []
separaLetra palavra letra = [antesLetra palavra letra, [letra], depoisLetra palavra letra]

antesLetra :: String -> Char -> String
antesLetra (x:xs) letra | x /= letra = [x] ++ antesLetra xs letra 
                        | otherwise = []
antesLetra [] _ = []

depoisLetra :: String -> Char -> String
depoisLetra (x:xs) letra | x == letra = xs
                         | otherwise = depoisLetra xs letra
depoisLetra [] _ = []

-- Ordena lista

-- [58,96,27,32,49]
-- [27,32,49,58,96]

sort :: Ord a => [a] -> [a]
sort [] = []
sort (a:b) = sort [x | x <- b, x<a]
             ++ [a] ++
             sort [x | x <- b, x>= a]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

quadradoMultiplicadoPorDois :: Int -> Int
quadradoMultiplicadoPorDois x =
    let quadrado = x * x
        resultado = quadrado * 2
    in resultado

gerarNumeros :: Int -> [Int]
gerarNumeros n = [x | x <- [1..n]]

maiorNum :: Ord a => [a] -> a

maiorNum (x:[]) = x
maiorNum lista = pegarMaior lista (head lista)

pegarMaior :: Ord a => [a] -> a -> a
pegarMaior [] numAtual = numAtual
pegarMaior (x:xs) numAtual | x < numAtual = pegarMaior xs numAtual
                           | otherwise = pegarMaior xs x 

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)