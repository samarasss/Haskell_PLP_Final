import Textos
import PreencheMatrizCM

-- Definindo tipos
type Coordenadas = (Int, Int)
type Valor = Int
type Elem = (Coordenadas,Valor)
type Matriz = [Elem]

--Corresponde a Função preencher Campos Vazio do COD em C 
--Função cria uma Matriz axb, onde a tupla (x, y) são as coordenadas na matriz e c eh o valor da posição.
criaMatriz:: Int -> Int -> Int -> Matriz
criaMatriz a b c = [((x,y), c) | x <-[1,2..a], y <-[1,2..b]]

--Função que gera as bombas 
bomba:: Int -> Int -> Matriz -> Matriz-> Matriz
bomba a b [] anterior = anterior
bomba a b (((x, y), z): mtz) anterior = if(a == x && b == y) 
											then anterior++[((x, y), -1)]++mtz 
										else bomba a b mtz (anterior++[((x, y), z)])

--Função que chama n vezes a Função Bomba
forBomba:: Int -> [(Int, Int)] -> Matriz -> Matriz
forBomba 0 [] mtz = mtz
forBomba index ((x, y): lista) mtz =  forBomba (index-1) lista (bomba x y mtz []) 

--Função que encontra as minas e chama a função soma adjacentes
minasAdjacentes:: Matriz -> Matriz -> Matriz
minasAdjacentes [] matriz = matriz
minasAdjacentes (((x, y), z): mtz) matriz = if (z == -1) then minasAdjacentes mtz (somaAdjacentes x y matriz) else (minasAdjacentes mtz matriz)

--Função que Inverte
inverte:: Int -> Int -> Matriz -> Matriz-> Matriz
inverte a b mtz anterior = reverse (verificaSoma a b mtz anterior)

--Função que Verifica se a posição da matriz não eh uma bomba e soma + 1
verificaSoma:: Int -> Int -> Matriz -> Matriz-> Matriz
verificaSoma a b [] anterior = [ ]
verificaSoma a b (((x, y), z): mtz) anterior = if(a == x && b == y && z /= -1) then anterior++(reverse ([((x,y), z+1)]++ mtz)) else verificaSoma a b mtz  anterior++[((x,y), z)]

--Função que soma os adjacentes a uma bomba
somaAdjacentes:: Int -> Int -> Matriz -> Matriz--campo minado e usuario e retorna a matriz usuario mostrando o elemento da posicao indicada.
somaAdjacentes 1 1 mtz = inverte 1 2 (inverte 2 1 (inverte 2 2 mtz [])[])[]
somaAdjacentes 1 y mtz = inverte 1 (y+1)(inverte 1 (y-1) (inverte 2 y (inverte 2 (y+1) (inverte 2 (y-1) mtz [])[])[])[])[]
somaAdjacentes 1 9 mtz = inverte 1 8 (inverte 2 9 (inverte 2 8 mtz []) [])[]
somaAdjacentes 9 1 mtz = inverte 9 2 (inverte 8 1 (inverte 8 2 mtz [])[])[]
somaAdjacentes 9 y mtz = inverte 9 (y-1) (inverte 9 (y+1) (inverte 8 y (inverte 8 (y+1) (inverte 8 (y-1) mtz [])[])[])[])[]
somaAdjacentes 9 9 mtz = inverte 9 8 (inverte 8 9 (inverte 8 8 mtz [])[])[]
somaAdjacentes x 9 mtz = inverte x 8 (inverte (x-1) 9 (inverte (x-1) 8 (inverte (x+1) 9 (inverte (x+1) 8 mtz [])[])[])[])[]
somaAdjacentes x y mtz = inverte x (y-1) (inverte x (y+1) (inverte (x-1) y (inverte (x-1) (y+1) (inverte (x-1) (y-1) (inverte (x+1) y (inverte (x+1) (y+1) (inverte (x+1) (y-1) mtz [])[])[])[])[])[])[])[]

imprimeLista e = do
	putStrLn (imprimeMatriz (listaLinha 9 1 e))
	putStrLn (imprimeMatriz (listaLinha 9 2 e))
	putStrLn (imprimeMatriz (listaLinha 9 3 e))
	putStrLn (imprimeMatriz (listaLinha 9 4 e))
	putStrLn (imprimeMatriz (listaLinha 9 5 e))
	putStrLn (imprimeMatriz (listaLinha 9 6 e))
	putStrLn (imprimeMatriz (listaLinha 9 7 e))
	putStrLn (imprimeMatriz (listaLinha 9 8 e))
	putStrLn (imprimeMatriz (listaLinha 9 9 e))

listaLinha::Int -> Int-> Matriz -> [Int]
listaLinha 0 xz mtz = []
listaLinha n xz (((x, y), z): mtz) 
	|x==xz = z:listaLinha (n-1) xz mtz
	|otherwise = listaLinha n xz mtz

imprimeMatriz::[Int]->String
imprimeMatriz [] = "|"
imprimeMatriz (z: mtz)
	|z==0= imprimeMatriz mtz ++ "   |"
	|z==(-2)=imprimeMatriz mtz ++ " x |"
	|z== (-1) = imprimeMatriz mtz ++ " * |"
	|otherwise = imprimeMatriz mtz ++" "++show z++" |"

--Função que modifica a matriz que é mostrada ao usuário pela matriz original(matriz que contém todas as informações)
modificaTodaMatriz:: Matriz -> Matriz -> Matriz -> Matriz
modificaTodaMatriz [] mtz mtz_usuario = mtz_usuario
modificaTodaMatriz (((a,b), c):mtz_CM) mtz mtz_usuario = modificaTodaMatriz mtz_CM mtz (modificaMatriz a b mtz mtz_usuario)

--Função que recebe o valor das coordenadas x e y e passa para a função modifica 
modificaMatriz:: Int -> Int -> Matriz -> Matriz -> Matriz
modificaMatriz x y (((a,b), c):mtz) mtz_usuario = if (x == a && y == b) then ( (modifica x y c mtz_usuario [])) else modificaMatriz x y mtz mtz_usuario

--Função que modifica a matriz do usuário de acordo com as coordenadas recebidas
modifica:: Int -> Int -> Int -> Matriz -> Matriz -> Matriz 
modifica x y z (((a,b), c):mtz_usuario) anterior = if (x == a && y == b) then anterior++(([((x, y), z)]++mtz_usuario)) else modifica x y z mtz_usuario (anterior++[((a,b), c)])
 
--Função que recebe o valor da coordenada y e imprime a matriz resultante para o usuário, após a chamada de algumas funções
inputy::Matriz -> Matriz -> Int -> IO()
inputy mtz_CM mtz_Im x = do
	putStr "Digite o eixo y: "
	y <- getLine
	let num = read y :: Int
    	if (num<1 || num > 9) then do
             putStrLn "Numero invalido!\n"
             inputy mtz_CM mtz_Im x
    	else do
		let matriz_usuario = if(verificaSeTemBomba (x, num) mtz_CM) then modificaTodaMatriz mtz_CM mtz_CM mtz_Im else modificaMatriz x num mtz_CM mtz_Im 
		imprimeLista (reverse matriz_usuario)
		if(verificaMtz matriz_usuario) then putStrLn(Textos.textoPerdeu) else whileMain mtz_CM matriz_usuario 

--Função que recebe o valor da coordenada x e chama a função inputy
inputx::Matriz -> Matriz -> IO()
inputx mtz_CM mtz_Im = do
	putStr "Digite o eixo x: "
	x <- getLine
	let num = read x :: Int
    	if (num<1 || num > 9) then do
             putStrLn "Numero invalido!\n"
             inputx mtz_CM mtz_Im
    	else do
		putStrLn " "
		inputy mtz_CM mtz_Im num
		putStrLn " "

recebeAleatorio :: IO()
recebeAleatorio = do
    putStr "Digite um numero de 1 a 100: "
    elem <- getLine
    let num = read elem :: Int
    if (num < 1 ||num > 100) then do
             putStrLn "Numero invalido!\n"
             recebeAleatorio
    else do
	putStrLn " "
	let campo_minado = (criaMatriz 9 9 0)
	let valoresAleatorios = PreencheMatrizCM.getRandomInt 0 num 
	--Gera uma Matriz com bombas nas posições fornecidas por valoresAleatorios
	let campo_bomba = forBomba 8 valoresAleatorios campo_minado
	--Cria uma nova Matriz com os adjcentes das bombas somados +1
	let prep_campo_minado = minasAdjacentes campo_bomba campo_bomba
	let mtz_Impressa = (criaMatriz 9 9 (-2))
	let resultado = whileMain prep_campo_minado mtz_Impressa
	inputx prep_campo_minado mtz_Impressa

whileMain:: Matriz -> Matriz -> IO()
whileMain mtz mtz_usuario = if(verificaMtz mtz_usuario) then do putStrLn(Textos.textoGanhou) else do inputx mtz mtz_usuario

verificaMtz:: Matriz -> Bool
verificaMtz [] = True 
verificaMtz (((a, b), c):mtz_usuario) = if(c == -2) then False else verificaMtz mtz_usuario

verificaSeTemBomba:: (Int, Int) -> Matriz -> Bool
verificaSeTemBomba tupla [] = False
verificaSeTemBomba (x, y) (((a, b), c):mtz) = if (x == a && y == b && c == -1) then True else verificaSeTemBomba (x, y) mtz

main = do
	putStrLn Textos.textoInicio
   	--Cria uma Matriz 9x9 com valores 0
	let campo_minado = (criaMatriz 9 9 0)
	imprimeLista(campo_minado)
	putStr "\n\n\n"
	recebeAleatorio 

