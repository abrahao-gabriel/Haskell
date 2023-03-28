
-- LISTA 02 --
-- Crie  uma função que verifique se o tamanho de uma String é par ou não.Use Bool como retorno.

stringPar :: String -> Bool
stringPar texto = even(length texto)

--   Escreva uma função  que receba  um  vetor de Strings e retorne uma lista com todos os elementos em ordem reversa.

inverte :: [String] -> [String]
inverte xs = reverse [ reverse x | x <- xs]


-- Faça	uma	função que receba uma String e retorne True se esta	for	um palíndromo; caso	contrário, False.

verificaPalindromo :: String -> Bool
verificaPalindromo xs = xs == reverse xs


-- LISTA 03 ----------------------------------------------------------------

-- 3.4
removeVogais :: String -> String
removeVogais str = [c | c <- str, c `notElem` "aeiouAEIOU"]


-- Exercício 3.10

revNum :: String -> Int -> String
revNum s n = reverse (take n s) ++ drop n s