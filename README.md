
SNAKE

===========================================================================================

Jogo inspirado no clássico Snake, para a cadeira de Algoritmos e Programação - CIC (UFRGS). 
Desenvolvido por: Ricardo Gabriel Herdt e Paulo Renato Lanzarin.

#Dependências (debian / ubuntu):

#libncursesw5-dev

Compilação:
gcc snake.c -o snake -Wl,-rpath=lib -Llib -lncursesw -lmenu

Execução:
./snake

===========================================================================================

MENU:

	OPÇÕES OFERECIDAS:
		-NOVO JOGO: inicia novo jogo.
		-ABRIR JOGO SALVO: abre o último jogo salvo (se houver algum).
		-CRÉDITOS: mostra os créditos do jogo.
		-SAIR: interrompe a execução.

	OBS.: a interação com o menu se dá com as setas UP e DOWN do teclado
	e o uso da tecla ENTER para selecionar a opção desejada.

===========================================================================================

CONTROLES E COMANDOS DO JOGO:

	DIRECIONAIS:
	W ou seta UP do teclado: move para cima.
	D ou seta RIGHT do teclado: move para a direita.
	X ou seta DOWN do teclado: move para baixo.
	A ou seta LEFT do teclado: move para a esquerda.

	INTERAÇÃO:
	Q - encerra o jogo, voltando ao menu.
	C - carrega um jogo anteriormente salvo.
	G - salva o jogo atual.
	T - avança de nível (trapaça).
	R - reinicia o jogo, desde o primeiro nível.
	P - pausa o jogo. Para reiniciá-lo, aperte P novamente.

===========================================================================================

O JOGO:

O jogo se dá através de três níveis com dificuldade crescente. Em todos, o objetivo é
chegar ao tamanho almejado da cobra (indicado logo abaixo do cenário) no menor número
de passos (também indicado abaixo do cenário). A velocidade da cobra varia conforme o 
nível, assim como a quantidade de peças incrementadas na cobra por alimento coletado.
Deve-se atentar às barreiras presentes no cenário: o contato da cobra com as mesmas ou 
com as bordas do cenário (em amarelo) acarretará na morte da cobra e no fim de jogo.

OBJETIVO DO PRIMEIRO NÍVEL: 35

OBJETIVO DO SEGUNDO NÍVEL: 65

OBJETIVO DO TERCEIRO NÍVEL: 95

===========================================================================================

AVISOS:

MENSAGEM DE ERRO: "O CENÁRIO NÃO PÔDE SER ABERTO!"
	- Dos cenários: caso algum arquivo do cenário não esteja presente ou esteja mal 
	nomeado, o nível referente ao mesmo não poderá ser iniciado. Visto isso, verifique 
	se os arquivos "cenario1.txt", "cenario2.txt" e "cenario3.txt" estão presentes na 
	pasta do jogo ou se o seu jogo não se encontra em áreas com opções de acesso 
	restringidas ao usuário comum do sistema (e.g.: root).

MENSAGEM DE ERRO: "O SAVEGAME NAO PÔDE SER ABERTO!"
	- Dos savegames: verifique se o jogo não se encontra em áreas com opções de acesso 
	restringidas ao usuário comum do sistema (e.g.: root). Se isso acontecer, o jogo 
	pode não ser salvo, visto que não haverá permissão para criação de novos arquivos. 
	O jogo transcorrerá normalmente; no entanto, ele não poderá ser salvo corretamente.
	Verifique, também, se há um jogo salvo (arquivo "jogo_salvo.bin"). Se a afirmação 
	for negativa, você terá que salvar alguma partida antes de carregar um jogo.







