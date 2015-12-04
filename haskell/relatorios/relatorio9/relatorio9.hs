-- To compile, just run "ghci relatorio8.hs"
-- Gamma function aproximation for complex numbers.
-- gamma n = sqrt(2*pi / n) * (1/e * (n + 1/(12*n - (1/10*n)))) ** n

ratio n = n!!1 - head(n)
e = 1 + sum([1/product[1..n] | n <- take 1000 [1..]])
gamma n = sum([product[1..n-1]])

-- Calculate the sum of an arithmetic progression
sum_ap n = 
    (
        (length n) * (2*head(n) + ((length n - 1) * ratio n))
    ) `div` 2

-- Calculate the product of an arithmetic progression
prod_ap n = (ratio n)** fromIntegral (length n) *
    (
        (gamma ( (head(n) / ratio n) + fromIntegral (length n) ))
        / gamma (head(n) / ratio n)
    )
