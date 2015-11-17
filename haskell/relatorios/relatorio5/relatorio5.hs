-- Parte 1
-- Simples função que retorna um valor
-- dado um número de entrada.
-- Por exemplo, se zero, então retorna 1
f x = case x of
	0 -> 1
	1 -> 5
	2 -> 2
	_ -> 1

-- Algoritmo quicksort, dada uma lista
-- de elementos, o algoritmo organiza
-- os elementos de forma crescente.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort lt ++ [x] ++ quicksort ge where 
{ lt = [y | y <- xs, y < x]; ge = [y | y <- xs, y >= x] }

-- Parte 2
-- Lista com elementos de 1 até 1000
list = [1..1000]
lambda1 = map(\x->x) [1..1000]

-- Progressão aritmética de 1 a 99 de razão 3
ap = [1,4..99]
lambda2 = map(\x->x) [1,4..99]

-- Progressão geométrica de 50 termos de razão 2
gp x = [x*(2**(n-1)) | n <- [1..50]]

-- O n-ésimo elemento de uma lista de fatoriais
fat n = product [1..n]
