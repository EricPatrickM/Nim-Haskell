import Text.Read
import Data.Char (ord)

main :: IO();
imprimir :: Int -> [Int] -> IO ();
asterisco :: Int -> String;
filtro :: Int -> Int -> [Int] -> Bool
remover :: Int -> Int -> [Int] -> [Int]
criterio :: [Int] -> Bool

--IMPRIMIR
imprimir a (x:xs) = do
  putStrLn $ show (a+1) ++ ": " ++ asterisco x
  imprimir (a+1) xs
imprimir a _ = return ()

asterisco n = replicate n '*'


--FILTRO
filtro qnt k xs = if(qnt > 3 || qnt < 1 || k > 4 || k < 0) then False else if(qnt > xs!!k) then False else True

--REMOVER
remover qnt k xs = take k xs ++ [((xs!!k)-qnt)] ++ drop (k+1) xs


--PARADA
criterio [] = True
criterio (x:xs) = if (x <= 0) then criterio xs else False



loop lista jogador = do
    imprimir 0 lista
    print("Jogador: " ++ show jogador);
    print("Digite a quantidade:");
    input <- getLine;
    let intInput = read input :: Int;
    
    print("Digite a linha:");
    k <- getLine;
    let intK = (read k :: Int) - 1;
    
    if(filtro intInput intK lista==False)
        then do
            print("Valores invalidos!!");
            loop lista jogador;
        else do
            let novaLista = (remover intInput intK lista);
            let novoJogador = if (jogador == 1) then 2 else 1;
            putStr("\n");
            if ((criterio novaLista)==False) then loop novaLista novoJogador else print("JOGADOR: " ++ show jogador ++ " GANHOU");

main = loop [1,2,3,4,5] 1