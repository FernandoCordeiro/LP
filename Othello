import Control.Exception    -- Pacote para o controle de excessoes
import System.IO.Error      -- Pacote para entrada/saida
import Control.Concurrent   -- Pacote para funcao sleep
import System.Process       -- Pacote para a funcao clear
import Data.List

type Pontuacao = Int
type Tabuleiro = [Char]

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
{- MAIN -}
-- funcao apenas para inicializar o programa no terminal
main = menu

-- funcao que exibe o Menu
menu :: IO ()
menu = do
            clear
            putStrLn "-------------- OTHELLO --------------\n"
            putStrLn "              1. Jogar"
            putStrLn "              0. Sair"
            putStr "\n Digite a operacao desejada: "
            op <- getChar
            getChar
            executarOpcao op

-- funcao que executa a operacao do menu
executarOpcao '1' = novoJogo
executarOpcao '0' = do
                        putStrLn "\n Obrigado, volte sempre\n"
                        threadDelay 500000      -- sleep por 0,5 mi de seg = 0,5 seg
                        return ()
executarOpcao _   = do
                        putStrLn "\n OPCAO INVALIDA!! Tente novamente..."
                        threadDelay 1000000     
                        menu
                         
-- funcao para limpar o terminal (Windows)
clear = system "cls"

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
{- MODEL -}
novoJogo = do   
                putStrLn "\n Se prepare, o jogo esta prestes a comecar..."
                putStrLn " Voce sera as pecas pretas (P)\n"
                threadDelay 2000000     -- sleep por 1 mi de seg = 1 seg
                clear
                {-
                    Formato do tabuleiro, cada bloco sera um par ordenado do tipo (x,y),
                    com x = linha e y = coluna desejadas
                    Exemplo 3x3:
                        (1,1) | (1,2) | (1,3)   
                        ---------------------   
                        (2,1) | (2,2) | (2,3)  
                        ---------------------
                        (3,1) | (3,2) | (3,3)                           
                -}
                -- Cada turno tera uma flag informando de quem eh a vez
                rodarJogo 0  [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','B','P',' ',' ',' ',' ',' ',' ','P','B',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']            

-- Funcao que aplica os dados no jogo
--rodarJogo :: Jogador -> Tabuleiro -> IO Tabuleiro
rodarJogo jogador tabuleiro = do 
                                printJogo jogador tabuleiro
                                if (jogador == 0)
                                    then do
                                            putStrLn "\n\n Faca seu movimento (x,y) "
                                            putStr "   Linha x = "
                                            x <- getChar
                                            getChar
                                            putStr "   Coluna y = "
                                            y <- getChar
                                            getChar
                                            threadDelay 500000
                                             
                                    else do
                                            putStrLn "\n Seu oponente esta jogando, aguarde...\n"
                                            threadDelay 500000

-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
{- VIEW -}
-- Esta funca exibe o estado atual do tabuleiro
printJogo jogador tabu = do
                            clear
                            if (jogador == 0) 
                                then putStr "\t\t                     Sua vez,\n"
                                else putStr "\t\t                     Vez do oponete\n"
                            putStrLn ("\n\t\t   1     2     3     4     5     6     7     8 " ++ "\n\t\t" ++
                {-Linha 1-}     "1 " ++ (show(tabu!!0)) ++ " | " ++ (show(tabu!!1)) ++ " | " ++ (show(tabu!!2)) ++ " | " ++ (show(tabu!!3)) ++ " | " ++
                                (show(tabu!!4)) ++ " | " ++ (show(tabu!!5)) ++ " | " ++ (show(tabu!!6)) ++ " | " ++ (show(tabu!!7)) ++ 
                                "\n\t\t  ---------------------------------------------\n\t\t" ++
                {-Linha 2-}     "2 " ++ (show(tabu!!8)) ++ " | " ++ (show(tabu!!9)) ++ " | " ++ (show(tabu!!10)) ++ " | " ++ (show(tabu!!11)) ++ " | " ++
                                (show(tabu!!12)) ++ " | " ++ (show(tabu!!13)) ++ " | " ++ (show(tabu!!14)) ++ " | " ++ (show(tabu!!15)) ++ 
                                "\n\t\t  ---------------------------------------------\n\t\t" ++
                {-Linha 3-}     "3 " ++ (show(tabu!!16)) ++ " | " ++ (show(tabu!!17)) ++ " | " ++ (show(tabu!!18)) ++ " | " ++ (show(tabu!!19)) ++ " | " ++
                                (show(tabu!!20)) ++ " | " ++ (show(tabu!!21)) ++ " | " ++ (show(tabu!!22)) ++ " | " ++ (show(tabu!!23)) ++ 
                                "\n\t\t  ---------------------------------------------\n\t\t" ++
                {-Linha 4-}     "4 " ++ (show(tabu!!24)) ++ " | " ++ (show(tabu!!25)) ++ " | " ++ (show(tabu!!26)) ++ " | " ++ (show(tabu!!27)) ++ " | " ++
                                (show(tabu!!28)) ++ " | " ++ (show(tabu!!29)) ++ " | " ++ (show(tabu!!30)) ++ " | " ++ (show(tabu!!31)) ++ 
                                "\n\t\t  ---------------------------------------------\n\t\t" ++
                {-Linha 5-}     "5 " ++ (show(tabu!!32)) ++ " | " ++ (show(tabu!!33)) ++ " | " ++ (show(tabu!!34)) ++ " | " ++ (show(tabu!!35)) ++ " | " ++
                                (show(tabu!!36)) ++ " | " ++ (show(tabu!!37)) ++ " | " ++ (show(tabu!!38)) ++ " | " ++ (show(tabu!!39)) ++ 
                                "\n\t\t  ---------------------------------------------\n\t\t" ++
                {-Linha 6-}     "6 " ++ (show(tabu!!40)) ++ " | " ++ (show(tabu!!41)) ++ " | " ++ (show(tabu!!42)) ++ " | " ++ (show(tabu!!43)) ++ " | " ++
                                (show(tabu!!44)) ++ " | " ++ (show(tabu!!45)) ++ " | " ++ (show(tabu!!46)) ++ " | " ++ (show(tabu!!47)) ++ 
                                "\n\t\t  ---------------------------------------------\n\t\t" ++
                {-Linha 7-}     "7 " ++ (show(tabu!!48)) ++ " | " ++ (show(tabu!!49)) ++ " | " ++ (show(tabu!!50)) ++ " | " ++ (show(tabu!!51)) ++ " | " ++
                                (show(tabu!!52)) ++ " | " ++ (show(tabu!!53)) ++ " | " ++ (show(tabu!!54)) ++ " | " ++ (show(tabu!!55)) ++ 
                                "\n\t\t ---------------------------------------------\n\t\t" ++
                {-Linha 8-}     "8 " ++ (show(tabu!!56)) ++ " | " ++ (show(tabu!!57)) ++ " | " ++ (show(tabu!!58)) ++ " | " ++ (show(tabu!!59)) ++ " | " ++
                                (show(tabu!!60)) ++ " | " ++ (show(tabu!!61)) ++ " | " ++ (show(tabu!!62)) ++ " | " ++ (show(tabu!!63)) )
                               
                                             


-- ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
{- CONTROLLER -}
