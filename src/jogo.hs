module Jogo

where

import Data.Char

type Casa = Int

-- Estrutura do tabuleiro
casa col lin = col + lin * 8
linha s = s `div` 8
coluna s = s `mod` 8
limite s = -1 < s && s < 64

-- Tradução entre posição e notação do usuário e vice versa
notacao s = ['a'..'h'] !! coluna s : show (linha s + 1)
notpos t = casa (ord (head t) - ord 'a') (ord (head (tail t)) - ord '1')

-- Numeração dos quadrados do tabuleiro.

a8::Int; b8::Int; c8::Int; d8::Int; e8::Int; f8::Int; g8::Int; h8::Int
a7::Int; b7::Int; c7::Int; d7::Int; e7::Int; f7::Int; g7::Int; h7::Int
a6::Int; b6::Int; c6::Int; d6::Int; e6::Int; f6::Int; g6::Int; h6::Int
a5::Int; b5::Int; c5::Int; d5::Int; e5::Int; f5::Int; g5::Int; h5::Int
a4::Int; b4::Int; c4::Int; d4::Int; e4::Int; f4::Int; g4::Int; h4::Int
a3::Int; b3::Int; c3::Int; d3::Int; e3::Int; f3::Int; g3::Int; h3::Int
a2::Int; b2::Int; c2::Int; d2::Int; e2::Int; f2::Int; g2::Int; h2::Int
a1::Int; b1::Int; c1::Int; d1::Int; e1::Int; f1::Int; g1::Int; h1::Int

a8 = 56; b8 = 57; c8 = 58; d8 = 59; e8 = 60; f8 = 61; g8 = 62; h8 = 63
a7 = 48; b7 = 49; c7 = 50; d7 = 51; e7 = 52; f7 = 53; g7 = 54; h7 = 55
a6 = 40; b6 = 41; c6 = 42; d6 = 43; e6 = 44; f6 = 45; g6 = 46; h6 = 47
a5 = 32; b5 = 33; c5 = 34; d5 = 35; e5 = 36; f5 = 37; g5 = 38; h5 = 39
a4 = 24; b4 = 25; c4 = 26; d4 = 27; e4 = 28; f4 = 29; g4 = 30; h4 = 31
a3 = 16; b3 = 17; c3 = 18; d3 = 19; e3 = 20; f3 = 21; g3 = 22; h3 = 23
a2 = 08; b2 = 09; c2 = 10; d2 = 11; e2 = 12; f2 = 13; g2 = 14; h2 = 15
a1 = 00; b1 = 01; c1 = 02; d1 = 03; e1 = 04; f1 = 05; g1 = 06; h1 = 07

-- Tipos de movimento

frente s = if limite s && linha s < 7 then s + 8 else 99
tras s = if limite s && linha s > 0 then s - 8 else 99
esquerda s = if limite s && coluna s > 0 then s - 1 else 99
direita s = if limite s && coluna s < 7 then s + 1 else 99

diagfd = frente . direita
diagfe = frente . esquerda
diagtd = tras . direita
diagte = tras . esquerda

-- Funções de movimento 

movCavalo :: Int -> [Int]
movCavalo s = [x | x <- [diagfd (frente s), diagfe (frente s), diagfd (direita s), diagtd (direita s), diagtd (tras s), diagte (tras s), diagte (esquerda s), diagfe (esquerda s)] , limite x]

movRei s = [x | x <- [(frente s), (diagfd s), (direita s), (diagtd s), (tras s), (diagte s), (esquerda s), (diagfe s)] , limite x]

sli dir s l = if limite (dir s) then (dir s) : sli dir (dir s) l else []

movBispo s = (sli diagfd s []) ++ (sli diagfe s []) ++ (sli diagtd s []) ++ (sli diagte s [])
movTorre s = (sli frente s []) ++ (sli tras s []) ++ (sli direita s []) ++ (sli esquerda s [])
movDama s = (movBispo s) ++ (movTorre s)

-- Dados em relação as peças e suas numerações

data Tipo = Rei | Dama | Bispo | Cavalo | Torre | Peao
     deriving (Eq, Show, Ord)

data Cor = Branco | Preto | None deriving (Eq,Show,Ord)

type Peca = Int

rb = 1 :: Int; db = 2 :: Int; tb = 3 :: Int; bb =  4 :: Int; cb =  5 :: Int; pb =  6 :: Int
rp = 7 :: Int; dp = 8 :: Int; tp = 9 :: Int; bp = 10 :: Int; cp = 11 :: Int; pp = 12 :: Int

vazio = 99 :: Int    

tipo :: Peca -> Tipo
tipo m = [Rei,Dama,Torre,Bispo,Cavalo,Peao] !! ((m - 1) `mod` 6)

cor :: Peca -> Cor
cor 99 = None
cor m = if m < rp then Branco else Preto

-- Mapeando as peças e os quadrados do tabuleiro/colocando os ícones das peças (Unicode)

pPeca :: Peca -> String
pPeca 1 = "♚ "; pPeca 2 = "♛ "; pPeca 3 = "♜ "; pPeca  4 = "♝ "; pPeca  5 = "♞ "; pPeca  6 = "♟ "
pPeca 7 = "♔ "; pPeca 8 = "♕ "; pPeca 9 = "♖ "; pPeca 10 = "♗ "; pPeca 11 = "♘ "; pPeca 12 = "♙ "
pPeca 99 = "--"
pPeca x = "??"

type Tabuleiro = [(Peca,Casa)]

tab0 = [(tp,a8),(cp,b8),(bp,c8),(dp,d8),(rp,e8),(bp,f8),(cp,g8),(tp,h8)
       ,(pp,a7),(pp,b7),(pp,c7),(pp,d7),(pp,e7),(pp,f7),(pp,g7),(pp,h7)
       ,(pb,a2),(pb,b2),(pb,c2),(pb,d2),(pb,e2),(pb,f2),(pb,g2),(pb,h2)
       ,(tb,a1),(cb,b1),(bb,c1),(db,d1),(rb,e1),(bb,f1),(cb,g1),(tb,h1)
       ]


-- função que verifica se uma determinada casa no tabuleiro contém alguma peça. Retorna 99 se a casa estiver vazia ou a peça correspondente à casa.
verifCasa tab cas = let mm = [ m | (m,s) <- tab, s == cas ] in
               if mm == [] then 99 else head mm


vverifCasa tab cas = pPeca (verifCasa tab cas)

-- Printa o tabuleiro na tela
pTabuleiro tab = do  
   mapM_ (\t -> putStrLn (pTabuleiro' tab t)) linhas
   putStrLn "|"
      where
        pTabuleiro' brd listacasas = foldl (\acc x -> acc ++ " " ++ x) [] (map (vverifCasa tab) listacasas)
        linhas = [[a8..h8],[a7..h7],[a6..h6],[a5..h5],[a4..h4],[a3..h3],[a2..h2],[a1..h1]]

-- Movimento das peças 


type Move = (Casa,Casa)

-- Recebe um movimento e retorna sua notação em formato de string

notacaoMove :: Move -> String
notacaoMove mv = notacao s ++ notacao s' where (s,s') = mv

-- Recebe uma string contendo a notação de um movimento e converte para o tipo Move. Utiliza a função notpos para converter as substrings que representam as casas de xadrez de volta para a representação interna das casas.
notpMove :: String -> Move
notpMove str = (notpos (take 2 str), notpos (drop 2 str))

--Funções de movimento que pega uma peça de um tabuleiro e move para a casa destino, caso haja uma peça, ela some

mvPeca :: Tabuleiro -> Move -> Tabuleiro
mvPeca tab mv = mvPeca' tab mv tab

mvPeca' tab mv [] = [(verifCasa tab s, s')] where (s,s') = mv
mvPeca' tab mv tab'
   | cas == s   = mvPeca' tab mv xs
   | cas == s'  = mvPeca' tab mv xs
   | otherwise  = (m,cas):mvPeca' tab mv xs
   where (m,cas):xs = tab'
         (s,s')    = mv

mvsPeca :: Tabuleiro -> [Move] -> Tabuleiro
mvsPeca tab mvs
    | null xs  = mvPeca tab mv
    | otherwise = mvsPeca (mvPeca tab mv) xs
    where mv:xs = mvs

-- Pseudomovimentos para testar, aqui as regras são deixadas de lado a fim de verificar se uma peça tem alguma possibilidade de movimento a partir de uma posição específica.

testa :: Tabuleiro -> Casa -> [Move]
testa tab cas = case t of
                   Cavalo   -> testaC tab cas
                   Bispo    -> testaB tab cas
                   Torre    -> testaT tab cas
                   Dama     -> testaD tab cas
                   Peao     -> testaP tab cas
                   Rei      -> testaR tab cas
                 where t = tipo (verifCasa tab cas)

testaC :: Tabuleiro -> Casa -> [Move]
testaC tab cas = filter (difCor tab) (map (\s -> (cas,s)) (movCavalo cas))

testaR :: Tabuleiro -> Casa -> [Move]
testaR tab cas = filter (difCor tab) (map (\s -> (cas,s)) (movRei cas))

difCor tab mv = cor (verifCasa tab s1) /= cor (verifCasa tab s2) where (s1,s2) = mv

testaB :: Tabuleiro -> Casa -> [Move]
testaB tab cas = f diagfd ++ f diagfe ++ f diagtd ++ f diagte where f = translado tab cas cas

testaT :: Tabuleiro -> Casa -> [Move]
testaT tab cas = f frente ++ f tras ++ f direita ++ f esquerda where f = translado tab cas cas

testaD :: Tabuleiro -> Casa -> [Move]
testaD tab cas = testaB tab cas ++ testaT tab cas

translado :: Tabuleiro -> Casa -> Casa -> (Casa -> Casa) -> [Move]
translado tab cas s dir
    | limite s' && casaVazia tab s'    = (cas,s'):translado tab cas s' dir
    | limite s' && difCor tab (cas,s') = [(cas,s')]
    | otherwise                        = []
    where s' = dir s

-- Retorna True se a casa no tabuleiro estiver vazia (não contém peça), ou False caso contrário.
casaVazia :: Tabuleiro -> Casa -> Bool
casaVazia tab cas = cor (verifCasa tab cas) == None

-- Retorna uma lista de todas as casas ocupadas por peças da cor especificada no tabuleiro.
pecaEm :: Tabuleiro -> Cor -> [Casa]
pecaEm tab c = [s | (m,s) <- tab, cor m == c]

-- Gera pseudomovimentos para um peão a partir de uma posição específica. Leva em consideração a cor do peão para determinar a direção dos movimentos.

testaP :: Tabuleiro -> Casa -> [Move]
testaP tab cas
   | cor (verifCasa tab cas) == Branco  = testaP' tab cas (linha a2) diagfd frente diagfe
   | cor (verifCasa tab cas) == Preto  = testaP' tab cas (linha a7) diagtd tras diagte
   | otherwise = error "testaP: casa não pode estar vazia"

-- Gera pseudomovimentos para um peão a partir de uma posição específica, considerando a cor do peão e as regras específicas de movimento dos peões (avanço normal, captura diagonal, avanço duplo inicial)

testaP' :: Tabuleiro -> Casa -> Int -> (Casa -> Casa) -> (Casa -> Casa) -> (Casa -> Casa) -> [Move]
testaP' tab cas home dr d dl =
   (if (linha cas) == home && casaVazia tab (d cas) && casaVazia tab (d (d cas)) then [(cas,(d (d cas)))] else []) ++
   (if casaVazia tab (d cas) then [(cas,(d cas))] else []) ++
   (if oponenteEm tab cas (dr cas) then [(cas,(dr cas))] else []) ++
   (if oponenteEm tab cas (dl cas) then [(cas,(dl cas))] else [])

oponenteEm :: Tabuleiro -> Casa -> Casa -> Bool
oponenteEm tab cas s' = cor (verifCasa tab s') /= None && cor (verifCasa tab s') /= cor (verifCasa tab cas)

-- Determina se uma casa está sob ameaça por uma peça de uma cor específica.

ofensas :: Tabuleiro -> Casa -> Cor -> Bool
ofensas tab cas c = ofensaR tab cas c || ofensaBD tab cas c || ofensaTD tab cas c || ofensaP tab cas c

ofensaR :: Tabuleiro -> Casa -> Cor -> Bool
ofensaR tab cas c = f frente || f tras || f direita || f esquerda || f diagfd || f diagfe || f diagtd || f diagte where f = ofensaR' tab cas c

ofensaR' :: Tabuleiro -> Casa -> Cor -> (Casa -> Casa) -> Bool
ofensaR' tab cas c dir = cor m == c && tipo m == Rei where m = verifCasa tab (dir cas)

ofensaBD :: Tabuleiro -> Casa -> Cor -> Bool
ofensaBD tab cas c = f diagfd || f diagfe || f diagtd || f diagte where f = ofensaBD' tab cas c

ofensaBD' :: Tabuleiro -> Casa -> Cor -> (Casa -> Casa) -> Bool
ofensaBD' tab cas c dir = cor m == c && tipo m `elem` [Bispo,Dama] where m = verifCasa tab (busca tab cas dir)

ofensaTD :: Tabuleiro -> Casa -> Cor -> Bool
ofensaTD tab cas c = f frente || f tras || f direita || f esquerda where f = ofensaTD' tab cas c

ofensaTD' :: Tabuleiro -> Casa -> Cor -> (Casa -> Casa) -> Bool
ofensaTD' tab cas c dir = cor m == c && tipo m `elem` [Torre,Dama] where m = verifCasa tab (busca tab cas dir)

ofensaP :: Tabuleiro -> Casa -> Cor -> Bool
ofensaP tab cas c
   | c == Branco = ofensaP' tab (diagtd cas) c || ofensaP' tab (diagte cas) c
   | c == Preto = ofensaP' tab (diagfd cas) c || ofensaP' tab (diagfe cas) c

ofensaP' tab cas c = cor m == c && tipo m == Peao where m = verifCasa tab cas

-- Função de busca, se a próxima casa na direção estiver fora do tabuleiro, a função retorna 99. Se a próxima casa estiver vazia, a função continua a busca. Se a próxima casa contiver uma peça, a função retorna essa casa.

busca :: Tabuleiro -> Casa -> (Casa -> Casa) -> Casa
busca tab cas dir
   | dir cas == 99                  = 99                      
   | verifCasa tab (dir cas) == 99      = busca tab (dir cas) dir    
   | otherwise                     = dir cas                   

-- Calcula os movimentos que põe o rei em xeque
-- Verifica se uma jogada deixa o rei adversário em xeque

oposto Branco = Preto
oposto Preto = Branco

-- Retorna a posição do rei de uma cor específica no tabuleiro.

posicaoRei :: Tabuleiro -> Cor -> Casa
posicaoRei tab corRei = head [s | (m, s) <- tab, cor m == corRei, tipo m == Rei]

-- Verifica se o rei de uma determinada cor está em xeque.

xeque :: Tabuleiro -> Cor -> Bool
xeque tab corRei =
    let posRei = posicaoRei tab corRei
        adversario = if corRei == Branco then Preto else Branco
        movimentosAdversario = concatMap (testa tab) (pecaEm tab adversario)
    in any (\mv -> snd mv == posRei) movimentosAdversario

-- Retorna uma lista de todos os possíveis movimentos para as peças de uma cor específica.

possiveisMovimentos :: Tabuleiro -> Cor -> [(Casa, Casa)]
possiveisMovimentos tab corJogador =
    concatMap (\cas -> map (\mv -> (cas, snd mv)) (testa tab cas)) (pecaEm tab corJogador)


xequeMate :: Tabuleiro -> Cor -> Bool
xequeMate tab corRei =
    let posRei = posicaoRei tab corRei
        movimentosPossiveis = possiveisMovimentos tab corRei
        tabAposMovimento mv = mvPeca tab mv
        todosMovimentosXeque = map (\mv -> xeque (tabAposMovimento mv) corRei) movimentosPossiveis
    in xeque tab corRei && all (== True) todosMovimentosXeque


-- Seguem as funções que verificam as jogadas válidas, como:
-- comer a própria peça, jogar pra fora do tabuleiro, movimentos errôneos, etc
valida :: Tabuleiro -> Cor -> [Move]
valida tab c = valida' tab tab c

-- função auxiliar que percorre as peças no tabuleiro e gera movimentos válidos para cada uma.

valida' :: Tabuleiro -> [(Peca,Casa)] -> Cor -> [Move]
valida' tab [] c = []
valida' tab peças c
   | cor m == c    = (testa tab s) ++ (valida' tab xs c)
   | otherwise     = valida' tab xs c
   where (m,s):xs = peças

validos :: Tabuleiro -> Cor -> [Move]
validos tab c =  (valida tab c)

