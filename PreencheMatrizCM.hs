module PreencheMatrizCM
(
matriz
,ran
,add
,getRandomInt
)where

import System.IO.Unsafe

--Funcao que gera a matriz de posicoes
matriz:: [(Int, Int)]
matriz = [(x, y) | x <-[1, 2.. 9], y <- [1, 2.. 9]]

--Funcao que gera numeros aletorio entre um intervalo
ran:: Int -> Int 
ran escolhido = if(escolhido `mod` 2 == 0) then ((escolhido * 77) `mod` 81) else ((escolhido * 43) `mod` 81) 

--Funcao que adiciona elemento na lista de tuplas 
add:: Int -> Int -> [(Int, Int)] -> (Int, Int)
add tam index (a: lista) = if(index == tam) then a else add tam (index + 1) lista

--Funcao que gera uma lista de tuplas de numeros aletorios
getRandomInt:: Int -> Int -> [(Int, Int)]
getRandomInt 0 escolhido = [add (ran escolhido) 0 matriz]++(getRandomInt 1 escolhido)   
getRandomInt 7 escolhido = [add (ran (escolhido+7)) 0 matriz] 
getRandomInt index escolhido =[add (ran (escolhido*index)) 0 matriz]++(getRandomInt (index+1) escolhido)


