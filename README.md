# Xadrez em Haskell 

## Proposta de projeto:

Este projeto visa implementar um jogo de xadrez em Haskell, um popular jogo de tabuleiro estratégico para dois jogadores. O jogo seguirá as regras clássicas do xadrez, permitindo movimentos legais de todas as peças e detectando xeques e xeques-mates.

## Como rodar o projeto:

Certifique-se de configurar a página de código para UTF-8 no prompt de comando do Windows. Utilize o seguinte comando:

```
$ chcp.com 65001
```
Para rodar, basta executar:
```
$ cd src
$ ghc -O partida.hs
```
## Complexidade e Desafios

Lógica do Jogo: Implementar as regras precisas do xadrez, incluindo movimentos válidos para cada peça, condições de xeque e xeque-mate, promoção de peões, entre outras regras fundamentais.

Representação do Tabuleiro: Criar uma estrutura eficiente para representar o tabuleiro de xadrez e garantir a integridade das posições das peças.

Entrada/Saída de Dados: Desenvolver mecanismos para entrada e saída de dados, permitindo que os jogadores realizem seus movimentos de forma intuitiva.

Interatividade: Projetar uma interface de usuário que seja amigável e forneça feedback adequado ao jogador, incluindo indicações visuais das jogadas possíveis.

Eficiência: Buscar otimizações e eficiência no código, garantindo um desempenho adequado, especialmente ao lidar com jogos mais complexos.


