module Partida

where

import System.IO
import Jogo


main :: IO ()
main = do
    putStrLn "XADREZ HASKELL!"
    intro tab0 Branco

intro :: Tabuleiro -> Cor -> IO ()
intro tab lado = do
    putStrLn "Tabuleiro atual:"
    pTabuleiro tab
    putStrLn $ show lado ++ " joga."
    let adversario = oposto lado
    if xequeMate tab lado
        then do
            putStrLn $ "Xeque-mate! " ++ show (oposto lado) ++ " vence!"
        else if xeque tab adversario
            then do
                putStrLn $ "Xeque para " ++ show adversario ++ "!"
                comandoXeque tab lado
            else comando tab lado

comandoXeque :: Tabuleiro -> Cor -> IO ()
comandoXeque tab lado = do
    putStr "Sua jogada: "
    cmd <- getLine
    let novoTab = mvPeca tab (notpMove cmd)
    let adversario = oposto lado
    if xequeMate novoTab lado
        then do
            putStrLn $ "Xeque-mate! " ++ show lado ++ " vence!"
        else do
            if xeque novoTab adversario
                then do
                    putStrLn $ "Xeque para " ++ show adversario ++ "!"
                    intro novoTab (oposto lado)
                else intro novoTab (oposto lado)

comando :: Tabuleiro -> Cor -> IO ()
comando tab lado = do
    putStr "Sua jogada: "
    cmd <- getLine
    if executa tab lado cmd
        then do
            let novoTab = mvPeca tab (notpMove cmd)
            pTabuleiro novoTab
            intro novoTab (oposto lado)
        else do
            putStrLn "Jogada inválida."
            comando tab lado

-- Verifica se a jogada é válida para o jogador atual
executa :: Tabuleiro -> Cor -> String -> Bool
executa tab lado cmd = notpMove cmd `elem` validos tab lado