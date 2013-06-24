#include <ncurses.h>
#include <termios.h> /* biblioteca para comando tcflush */
#include <stdlib.h>
#include <unistd.h> /* usleep */
#include <locale.h> /* para codificar em Unicode */
#include <wchar.h>
#include <string.h>
#include <menu.h> /* biblioteca de menu do ncurses */
#define MAXTAM 105 /* tamanho máximo da cobra */
#define MAXY 24 
#define MAXX 80
#define HEAD 'Q'
#define CORPO '#'
#define TAMINIC 5
#define ENTER 10
#define ARRAY_SIZE(a) (sizeof(a) / sizeof(a[0])) /* macro para determinar tamanho do array de opções */


WINDOW *jogo_win; /* janela principal do jogo */
WINDOW *info_win; /* janela inferior esquerda, exibe informações úteis ao usuário */
WINDOW *aviso_win; /* janela inferior direita, exibe avisos importantes ao usuário */
WINDOW *menu1_win; /* janela que abriga o menu principal */

/* estrutura para coordenadas da cobra */
struct pos{
  int x; 
  int y;
};

typedef struct pos pos;

 /* estrutura para configurações do nível (incremento da cobra e velocidade) */
struct levelSettings{
  int velocidade;
  int unidadesInc; /* unidades de incremento */
};

/* estrutura para os dados da cobra */
struct snakeData {
  struct pos cobra[MAXTAM]; /* array de pos, formando o corpo inteiro da cobra */
  struct pos posInc; /* posição de movimento da cobra */
  int tam; /* tamanho da cobra */
};

/* estrutura para dados do nível */
struct roundData { 
  int passos; /* contador de passos */
  int alimCount; /* contador de alimentos */
  int nivel; /* controle de nível atual */
  struct pos alimentos[29]; /* alimentos do mapa */
};

typedef struct snakeData snakeData;

typedef struct roundData roundData;


void play(snakeData *thisSnake, int *sair, struct levelSettings *levelSettings, roundData *thisRound, FILE *cenario, FILE *savegame);

void storeGame(FILE *savegame, struct roundData *thisRound, struct snakeData *thisSnake, struct levelSettings *levelSettings);

void loadGame(FILE *savegame, struct roundData *thisRound, struct snakeData *thisSnake, struct levelSettings *levelSettings);

void checaCenario(FILE *cenario, roundData *thisRound, snakeData *thisSnake, int *sair);

void setNivel(roundData *thisRound, FILE *cenario, struct levelSettings *levelSettings, snakeData *thisSnake, int *sair);

void desenhaCenario(FILE *cenario, struct pos *alimentos);

void imprimeInfos(roundData *thisRound, snakeData *thisSnake);

void initCobra(struct pos *cobra, int *tam);

void scanPos(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings, roundData *thisRound, int *sair);

void morre(roundData *thisRound);

void addAlimento(struct pos *alimentos, int *alimCount);

void desenhaCobra(struct pos *cobra, int *tam);

void incCobra(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings);

void input(snakeData *thisSnake, roundData *thisRound, FILE *cenario, struct levelSettings *levelSettings, int *sair, FILE *savegame);

void moveCobra(snakeData *thisSnake, struct levelSettings *levelSettings, roundData *thisRound, int *sair);

int opGet(void);	

void menu(snakeData *thisSnake, int *sair, struct levelSettings *levelSettings, roundData *thisRound, FILE *cenario, FILE *savegame);

int main(int argc, char *argv[])
{
  int i, sair=0;

  snakeData thisSnake;
  roundData thisRound;
  struct levelSettings levelSettings;
  struct pos alimentos[29];
  FILE *cenario;
  FILE *savegame;

  thisRound.nivel = 0;
  thisRound.passos = 0;

  system("resize -s 30 83"); /* define o tamanho do terminal */
  setlocale(LC_ALL,""); /* Unicode */
  initscr(); /* inicializa o modo curses */
  keypad(stdscr, TRUE); /* possibilita o uso das setas do teclado */
  clear();
  noecho(); /* evita que teclas digitadas sejam impressas na tela */
  cbreak(); /* desabilita buffer de linha */
  curs_set(0); /* esconde o cursor */

 
  if(has_colors() == FALSE)
    {
      endwin();
      printf("Seu terminal não suporta cores\n");
      exit(1);
    }
  start_color();
  init_pair(1, COLOR_CYAN, COLOR_BLACK); /* conjunto de cores */
  init_pair(2, COLOR_RED, COLOR_BLACK);
  init_pair(3, COLOR_YELLOW, COLOR_BLACK);

  keypad(jogo_win, TRUE);
  jogo_win = newwin(MAXY+1, MAXX+1, 1, 1); /* janela do jogo */
  aviso_win = newwin(2, 35, 27, 47); /* janela de avisos */
  info_win = newwin(2, 35, 27, 5); /* janela de informações */
  menu1_win = newwin(MAXY+1, MAXX+1, 1, 1); /* janela do menu */
  refresh();
  menu(&thisSnake, &sair, &levelSettings, &thisRound, cenario, savegame);
  endwin();
  return 0;
}

/* loop principal de execução do jogo */
void play(snakeData *thisSnake, int *sair, struct levelSettings *levelSettings, roundData *thisRound, FILE *cenario, FILE *savegame)
{
  while(!(*sair))
    {
      timeout(0);
      if((thisSnake->tam) >= (30*(thisRound->nivel)+TAMINIC)) /* testa se o usuário já chegou ao objetivo */
      /* if(thisRound->alimCount >= 31)/\* testa se o usuário já chegou ao objetivo *\/ */
	setNivel(thisRound, cenario, levelSettings, thisSnake, sair); /* constrói novo nível */
      input(thisSnake, thisRound, cenario, levelSettings, sair, savegame); 
      moveCobra(thisSnake, levelSettings, thisRound, sair);
      imprimeInfos(thisRound, thisSnake);
      tcflush(thisSnake->posInc.x, TCIFLUSH); /* limpa buffer de entrada do terminal */
      tcflush(thisSnake->posInc.y, TCIFLUSH); /* limpa buffer de entrada do terminal */
      usleep(levelSettings->velocidade); /* garante a velocidade da cobra */
    }
  morre(thisRound); /* volta ao menu */
}

/* SALVA OS SEGUINTES ATRIBUTOS EM jogo_salvo.bin:
   -thisRound  -levelSettings
   -tam   -posInc.x  -posInc.y  */
void storeGame(FILE *savegame, struct roundData *thisRound, struct snakeData *thisSnake, struct levelSettings *levelSettings)
{
  int i;
  wclear(aviso_win);
  savegame = fopen("jogo_salvo.bin", "w+b");
  if(savegame != NULL)
    {
      fwrite(levelSettings, sizeof(*levelSettings), 1, savegame); /* salva estrutura inteira */
      fwrite(thisRound, sizeof(roundData), 1, savegame); /* salva estrutura inteira */
      fwrite(&(thisSnake->tam), sizeof(int), 1, savegame); /* salva tamanho da cobra */
      fwrite(&(thisSnake->posInc), sizeof(pos), 1, savegame); /* salva direção em que a cobra se movimentava */
      fwrite(&(thisSnake->cobra), sizeof(pos), thisSnake->tam, savegame);
      putwin(jogo_win, savegame); /* salva o cenário/evita rebuilding do mesmo */
      wprintw(aviso_win, "O JOGO FOI SALVO!");
      fclose(savegame);
    }
  else
    {
      wclear(aviso_win);
      wprintw(aviso_win, "O SAVEGAME NAO PODE SER ABERTO!");
      wrefresh(aviso_win);
      usleep(2000000);
    }
  wrefresh(aviso_win);
}

/* CARREGA OS SEGUINTES ATRIBUTOS DE jogo_salvo.bin:
   -thisRound  -levelSettings
   -tam   -posInc.x  -posInc.y  */
void loadGame(FILE *savegame, struct roundData *thisRound, struct snakeData *thisSnake, struct levelSettings *levelSettings)
{
  int i;
  wclear(aviso_win);
  savegame = fopen("jogo_salvo.bin", "rb");
  if(savegame != NULL)
    {
      fread(levelSettings, sizeof(*levelSettings), 1, savegame); /* lê os settings do nível */
      fread(thisRound, sizeof(roundData), 1, savegame); /* lê as informações referentes ao estado do jogo */
      fread(&(thisSnake->tam), sizeof(int), 1, savegame); /* lê tamanho da cobra */
      fread(&(thisSnake->posInc), sizeof(pos), 1, savegame); /* lê direção de movimentação da cobra */
      fread(&(thisSnake->cobra), sizeof(pos), thisSnake->tam, savegame); /*posição das partes da cobra */
      jogo_win = getwin(savegame); /* restaura cenário/evita rebuilding do mesmo */
      wprintw(aviso_win, "JOGO CARREGADO!");
      fclose(savegame);
    }
  else 
    {
      wclear(aviso_win);
      wprintw(aviso_win, "O SAVEGAME NAO PÔDE SER ABERTO!\n");
      wrefresh(aviso_win);
      usleep(2000000);
    }

  wrefresh(aviso_win);
}

/* checa consistência do arquivo cenario'x'.txt*/
void checaCenario(FILE *cenario, roundData *thisRound, snakeData *thisSnake, int *sair)
{
  if(cenario != NULL)
    {
      desenhaCenario(cenario, thisRound->alimentos); /* desenha novo cenário */
      addAlimento(thisRound->alimentos, &(thisRound->alimCount)); /* adiciona alimento */
      initCobra(thisSnake->cobra, &(thisSnake->tam)); /* reinicia a cobra */
      wmove(jogo_win, thisSnake->cobra[0].y, thisSnake->cobra[0].x); 
      desenhaCobra(thisSnake->cobra, &(thisSnake->tam));
    }
  else {
    *sair = 1;
    wclear(aviso_win);
    wprintw(aviso_win, "O CENÁRIO NÃO PÔDE SER ABERTO!");
    wrefresh(aviso_win);
    usleep(4000000);
  }
   
}

/* reseta todos os atributos da cobra, atualiza settings de nível (cenário, levelSettings), e inicia cenário */
void setNivel(roundData *thisRound, FILE *cenario, struct levelSettings *levelSettings, snakeData *thisSnake, int *sair)
{
  (thisRound->nivel)++;
  (thisRound->alimCount) = 0;
  (thisSnake->tam) = TAMINIC;
  thisSnake->posInc.x = 1;
  thisSnake->posInc.y = 0;
  if(thisRound->nivel <= 3){
    wclear(jogo_win);
    mvwprintw(jogo_win, 12, 34, "N  I  V  E  L  %d", thisRound->nivel); /* indica o próximo nível ao usuário */
    wrefresh(jogo_win);
    wclear(jogo_win); /* limpa o cenário antigo */
    usleep(1000000); /* delay de entrada do novo cenário */
    /* abertura do cenariox.txt é feita por aqui, de acordo com o nível*/
    if(thisRound->nivel == 1)
      {
	cenario = fopen("cenario1.txt", "r");
	checaCenario(cenario, thisRound, thisSnake, sair);
	levelSettings->velocidade = 100000;
	levelSettings->unidadesInc = 1;
      }
    else if(thisRound->nivel == 2)
      {
	cenario = fopen("cenario2.txt", "r");
	checaCenario(cenario, thisRound, thisSnake, sair);
	levelSettings->velocidade = 84500; 
	levelSettings->unidadesInc = 2; 
      }
    else if(thisRound->nivel == 3)
      {
	cenario = fopen("cenario3.txt", "r");
	checaCenario(cenario, thisRound, thisSnake, sair);
	levelSettings->velocidade = 69500;
	levelSettings->unidadesInc = 3;	
      }
  }
  else *sair = 1; /* volta ao menu */

}

/* desenha o cenário, presente num arquivo cenario'x'.txt, na janela jogo_win */
void desenhaCenario(FILE *cenario, struct pos *alimentos)
{
  int i, j, alimInd;
  int x, y, len;
  char obj;
  wchar_t mapa[MAXY+1][MAXX+1];
  wchar_t linha[MAXX+1];
  wchar_t *stopwcs, *temp1, *temp2;

  alimInd = 0; /* inicializa alimInd para controlar adição de alimentos */

  /* zera mapa */
  for(i=0; i<MAXY+1; i++)
    for(j=0; j<MAXX+1; j++)
      mapa[i][j] = L'0';

  /* Lê arquivo de cenário e armazena dados numa matriz */
  i=0;
  while(fgetws(linha, MAXX+1, cenario) != NULL)
    {    
      if(linha[0] > 48 && linha[0] < 57) /* é dígito: passa coordenadas de alimentos para o array */

      	{
	  temp1 = wcstok(linha, L" ,", &stopwcs); /* armazena coordenadas em variáveis temp*/
	  temp2 = wcstok(NULL, L" ,", &stopwcs);
	  x = wcstol(temp1, &stopwcs, 10); /* converte temp de wide char para inteiro */
	  y = wcstol(temp2, &stopwcs, 10);
      	  alimentos[alimInd].x = x;
      	  alimentos[alimInd].y = y;
      	  alimInd++;
      	}
      else {
      	wcscpy(mapa[i], linha);
	i++;
      }
    }
  wcscpy(mapa[1], mapa[23]); /* corrige bug da linha extra */
  //  wcscpy(mapa[0], mapa[24]); /* corrige bug da linha extra */

  wattron(jogo_win, COLOR_PAIR(3)); /* cor do cenário */

  /* desenha cenário propriamente dito */
  for(i=0; i<MAXY+1; i++)
    {
      /* mvwprintw(jogo_win, i, 0, "%ls", mapa[i]); */
      wmove(jogo_win, i, 0);
      waddnwstr(jogo_win, mapa[i], 80); /* imprime string armazenada no índice i da matriz mapa */
    }

  //  mvwprintw(jogo_win, 0, 79, "%ls", L"\u2588"); 
  wattroff(jogo_win, COLOR_PAIR(3)); /* restaura cor padrão */
  fclose(cenario);
  
  wrefresh(jogo_win);
}

/* imprime informações úteis ao usuário na janela info_win */
void imprimeInfos(roundData *thisRound, snakeData *thisSnake)
{
  int x, y;

  getyx(info_win, y, x);
  wmove(info_win, 0, 0);
  wprintw(info_win, "Tamanho da cobra: %d", (thisSnake->tam)); /*tamanho base + nivel*/
  wprintw(info_win, "  Passos: %d", thisRound->passos); /* contador de passos */
  wprintw(info_win, "\nObjetivo: %d", 30*(thisRound->nivel)+TAMINIC); /* tamanho da cobra almejado */
  wprintw(info_win, "  Nivel: %d", (thisRound->nivel)); /* nível atual */
  wmove(info_win, y, x);
  wrefresh(info_win);
}

/* inicializa a estrutura da cobra povoando o array cobra com as coordenadas */
/* centrais da tela (voltada horizontalmente para a direita). */
/* Adiciona-se mais um elemento ao fim da cobra, o qual será utilizado para */
/* finalizá-la com o caractere ' ' na função desenhaCobra */
void initCobra(struct pos *cobra, int *tam)
{
  int i;
  // getmaxyx(stdscr,maxY,maxX);

  cobra[0].x = MAXX / 2;
  cobra[0].y = MAXY / 2;
  for(i=1; i <= *tam; i++) /* <= pois há o fim da cobra */
    {
      cobra[i].x = cobra[i-1].x - 1;
      cobra[i].y = cobra[i-1].y;
    }
}

/* analisa elementos contidos na posição atual e chama as ações apropriadas */
void scanPos(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings, roundData *thisRound, int *sair)
{
  char elem;
  elem = (winch(jogo_win) & A_CHARTEXT);
  
  if(elem == -120 || elem == 35) /* Caractere bloco ou própria cobra */
    {
      *sair = 1;
    }
  else if (elem == 42) /* alimento */
    {
      incCobra(cobra, appendPos, tam, levelSettings);
      addAlimento(thisRound->alimentos, &(thisRound->alimCount));
    }
  wmove(jogo_win, cobra[0].y, cobra[0].x); /* restaura posição */
}

/* exibe a pontuação e níveis finalizados ao usuário.
   Após isso, volta ao menu */
void morre(roundData *thisRound)
{
  int flag;
  /* clear e refreshes para apagar janelas de informações */
  wclear(info_win); 
  wrefresh(info_win);
  wclear(aviso_win);
  wrefresh(aviso_win);
  wclear(jogo_win);
  mvwprintw(jogo_win, 10, 33, "G A M E  O V E R ");
  mvwprintw(jogo_win, 12, 36, "PASSOS: %d ", (thisRound->passos)); /* passos totais */
  mvwprintw(jogo_win, 14, 31, "NIVEIS FINALIZADOS: %d ", (thisRound->nivel) - 1); /* níveis FINALIZADOS */
  mvwprintw(jogo_win, 24, 1, "<ESC> PARA VOLTAR AO MENU ");
  wrefresh(jogo_win);
  do /* usuário tem de apertar ESC para voltar ao menu */
    {
      flag = toupper(getch());
    }
  while(flag != 27);
  thisRound->alimCount = 0;
  thisRound->nivel = 0;
  thisRound->passos = 0;
}

/* Atua na colocação de alimentos presentes no array no cenário
   Também há a validação e consistência da posição do alimento */
void addAlimento(struct pos *alimentos, int *alimCount)
{
  char elem;
  int valido=0, tentativa=0;
  int x, y; /* coordenadas */

  y = alimentos[*alimCount].y;
  x = alimentos[*alimCount].x;

  while(valido==0)
    {
      wmove(jogo_win, y, x);
      elem = (winch(jogo_win) & A_CHARTEXT);
      if(elem == 32 /* espaço */)
	{
	  wattron(jogo_win, COLOR_PAIR(2)); /* maçãs vermelhas */
	  waddch(jogo_win, '*');
	  wattroff(jogo_win, COLOR_PAIR(2));
	  valido = 1;
	}
      else
	{
	  switch(tentativa) /* caso posição seja inválida, testa as posições mais próximas */
	    {
	    case 0: /* testa à direita */
	      x++;
	      break;
	    case 1: /* testa acima */
	      x--; /* restaura x */
	      y--;
	      break;
	    case 2: /* testa à esquerda */
	      x--;
	      y++; /* restaura y */
	      break;
	    case 3: /* testa abaixo */
	      x++; /* restaura x */
	      y++;
	      break;
	    default:
	      valido = 1;
	    }
	  tentativa++;
	}
    }
  alimentos[*alimCount].x = x;
  alimentos[*alimCount].y = y;
  (*alimCount)++;

  wrefresh(jogo_win);
}

/* desenha a cobra através de um laço */
void desenhaCobra(struct pos *cobra, int *tam)
{
  int i, x, y;
  char elem;

  wattron(jogo_win, COLOR_PAIR(1)); /* habilita cor */
  waddch(jogo_win, HEAD); /* cabeça */
  for(i=1; i<(*tam-2); i++) /* corpo */
    {
      mvwaddch(jogo_win, cobra[i].y, cobra[i].x, CORPO);
    }
  wmove(jogo_win, cobra[(*tam)].y, cobra[(*tam)].x);
  elem = (winch(jogo_win) & A_CHARTEXT);
  if(elem != -120) /* é bloco */
    waddch(jogo_win, ' '); /* fim da cobra com espaço para 'limpar o rastro' */
  wattroff(jogo_win, COLOR_PAIR(1));
  wrefresh(jogo_win);
}

/* atua no incremento da cobra quando está ingere um alimento */
void incCobra(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings)
{
  int diffX, diffY, i;
  for(i=0; i<levelSettings->unidadesInc; i++)
    (*tam)++;
  cobra[*tam].x = appendPos->x; /* transmite antigas coordenadas do fim da cobra (obtidas de appendPos) ao novo fim */
  cobra[*tam].y = appendPos->y;
  diffX = cobra[*tam - 1].x - cobra[*tam].x; /* diffX e diffY calculam o sentido de crescimento */
  diffY = cobra[*tam - 1].y - cobra[*tam].y;
  for(i=1; i < levelSettings->unidadesInc; i++)
    {
      cobra[*tam + i].x = appendPos->x - diffX; /* transmite as coordenadas para unidadesInc-1 */
      cobra[*tam + i].y = appendPos->y - diffY; /* após a última parte da cobra */
    }
}

/* analisa a entrada do usuário e espera um tempo 
   indefinido até que o usuário entre algo válido */
void input(snakeData *thisSnake, roundData *thisRound, FILE *cenario, struct levelSettings *levelSettings, int *sair, FILE *savegame)
{
  int dir; /* tipo int necessário para reconhecer setas */

  dir = toupper(getch());
  switch(dir)
    {
      /* Ordenadas iniciam no topo superior da tela */
    case 'D':
    case KEY_RIGHT: // seta direita
      if(thisSnake->posInc.x != -1)
	{
	  thisSnake->posInc.x = 1;
	  thisSnake->posInc.y = 0; 
	}
      break;

    case 'A':
    case KEY_LEFT: // seta esquerda
      if(thisSnake->posInc.x != 1)
	{
	  thisSnake->posInc.x = -1;
	  thisSnake->posInc.y = 0;
	}
      break;

    case 'W':
    case KEY_UP: // seta "up"
      if(thisSnake->posInc.y != 1)
	{
	  thisSnake->posInc.x = 0;
	  thisSnake->posInc.y = -1;
	}
      break;

    case 'X':
    case KEY_DOWN: // seta "down"
      if(thisSnake->posInc.y != -1)
	{
	  thisSnake->posInc.x = 0;
	  thisSnake->posInc.y = 1;
	}
      break;

    case 'Q':
      {
	wclear(aviso_win);
	wprintw(aviso_win, "DESEJA MESMO SAIR? :( (S/N)");
	wrefresh(aviso_win);
	dir = opGet();
	if(dir == 'S') /* confirma saída do jogo */
	  *sair = 1;
	else if(dir == 'N') /* regride a saída do jogo */
	  timeout(0); /* descongela jogo */
      }
      break;

    case 'T': /* trapaça */
      {
	wclear(aviso_win);
	wprintw(aviso_win, "DESEJA MESMO TRAPACEAR? :( (S/N)");
	wrefresh(aviso_win);
	dir = opGet();
	if(dir == 'S')  /* confirma trapaça */
	  setNivel(thisRound, cenario, levelSettings, thisSnake, sair); /* avança nível */
	else if(dir == 'N') /* regride trapaça */
	  timeout(0); /* descongela jogo */
      }
      break;

    case 'G': /* salva jogo */
      storeGame(savegame, thisRound, thisSnake, levelSettings);
      break;

    case 'C': /*carrega jogo*/
      {
	wclear(aviso_win);
	wprintw(aviso_win, "DESEJA CARREGAR O JOGO (S/N)?");
	wrefresh(aviso_win);
	dir = opGet();
	if(dir == 'S')  /* confirma trapaça */
	  loadGame(savegame, thisRound, thisSnake, levelSettings); /*carrega jogo de facto */
	else if(dir == 'N') /* regride */
	  timeout(0); /* descongela jogo */
      }
      break;

    case 'R': /*reinicia jogo*/
      {
	thisRound->nivel = 0;
	thisRound->passos = 0;
	thisRound->alimCount = 0;
	setNivel(thisRound, cenario, levelSettings, thisSnake, sair);
      }
      break;

    case 'P': /*pausa jogo*/
      {
	timeout(-1);
	do
	  {
	    dir = toupper(getch());
	    if(dir == 'P')
	      timeout(0);
	  }
	while(dir != 'P');
      }
      break;
    }
  wclear(aviso_win);
  wrefresh(aviso_win);
}

/* atua somente na obtenção de confirmação do usuário
   quando solicitado a responder S/N */
int opGet(void)
{
  int flag;
  timeout(-1); /* pausa jogo */
  do
    {
      flag = toupper(getch());	 
    }
  while(flag != 'N' && flag != 'S');
  return flag;
}

/* atua no deslocamento da cobra no cenário */
void moveCobra(snakeData *thisSnake, struct levelSettings *levelSettings, roundData *thisRound, int *sair)
{
  int i;
  struct pos temp;
  int incFlag = 1;

  temp.x = thisSnake->cobra[thisSnake->tam].x; /* armazena coordenadas do final da cobra (uso em incCobra) */
  temp.y = thisSnake->cobra[thisSnake->tam].y;
  /* transfere as coordenadas dos elos de trás pra frente */
  for(i=thisSnake->tam; i>0; i--)
    {
      thisSnake->cobra[i].x = thisSnake->cobra[i-1].x;
      thisSnake->cobra[i].y = thisSnake->cobra[i-1].y;
    }

  thisSnake->cobra[0].x += thisSnake->posInc.x; /* calcula a posição da cabeça */
  thisSnake->cobra[0].y += thisSnake->posInc.y;
  wmove(jogo_win, thisSnake->cobra[0].y, thisSnake->cobra[0].x);
  (thisRound->passos)++;

  scanPos(thisSnake->cobra, &temp, &(thisSnake->tam), levelSettings, thisRound, sair); 
  if(!(*sair))
    desenhaCobra(thisSnake->cobra, &(thisSnake->tam));
}

/* função que imprime o menu na tela e o mantém na janela
   menu1_win até que o programa seja finalizado. É o laço maior
   que abrange a função play. */
void menu(snakeData *thisSnake, int *sair, struct levelSettings *levelSettings, roundData *thisRound, FILE *cenario, FILE *savegame)
{
  char *opcoes[] = {"NOVO JOGO", "ABRIR JOGO SALVO", "CREDITOS", "SAIR", (char *)NULL, " ",}; /* array para itens do menu */

  int input, numop, i, menuexit = 0;
  ITEM **itens, *itematual, *selec; /* **itens é o array do tipo ITEM */
  MENU *menu; /* variável menu */

  numop = ARRAY_SIZE(opcoes); /* número de itens do menu */
  itens = (ITEM **)calloc(numop, sizeof(ITEM *)); /*alocação de memória*/
  for(i = 0; i < numop; i++) /* armazena os itens na variável itens de tipo ITEM */
    itens[i] = new_item(opcoes[i], " ");
  itens[numop] = (ITEM *)NULL;
  menu = new_menu((ITEM **)itens); /* cria novo menu com os itens determinados no array opcoes */

  set_menu_win(menu, menu1_win); /* seta a janela do menu */
  set_menu_sub(menu, derwin(menu1_win, 8, 20, 10, 30)); /* seta a subjanela do menu (que serve às opções) */
  set_menu_mark(menu, "->"); /* apontador do menu (estético) */

  while(menuexit != 1)
    {
      /* Bordas do menu (estético) */
      box(menu1_win, 0, 0);	
      wattron(menu1_win, COLOR_PAIR(1));
      mvwprintw(menu1_win, 8, 32, "S   N   A   K   E");
      wattron(menu1_win, COLOR_PAIR(3));
      set_menu_fore(menu, COLOR_PAIR(3));
      wborder(menu1_win, '#', '#', '#', '#', 'Q', 'Q', 'Q', 'Q');
      wattroff(menu1_win, COLOR_PAIR(3));  
      refresh();
      post_menu(menu); /* imprime menu */
      wrefresh(menu1_win);
      input = getch();
      switch(input)
	{
	case KEY_DOWN:/* rola para baixo */
	  menu_driver(menu, REQ_DOWN_ITEM); /* menu_driver() e definição REQ_DOWN_ITEM nativos de menu.h */
	  break;
	case KEY_UP: /* rola para cima */
	  menu_driver(menu, REQ_UP_ITEM); /* menu_driver() e definição REQ_UP_ITEM nativos de menu.h */
	  break;
	case ENTER:
	  {
	    selec = current_item(menu); /* armazena o item selecionado pelo usuário */
	    if(item_name(selec) == "NOVO JOGO"){ /* item_name() analisa o nome do item dentro do array de strings */
	      *sair = 0;
	      wclear(jogo_win);
	      thisRound->nivel = 0;
	      setNivel(thisRound, cenario, levelSettings, thisSnake, sair);
	      play(thisSnake, sair, levelSettings, thisRound, cenario, savegame); /* inicia jogo / loop principal */
	    }
	    else if(item_name(selec) == "ABRIR JOGO SALVO"){ /* abre último jogo salvo */
	      wclear(jogo_win);
	      *sair = 0;
	      savegame = fopen("jogo_salvo.bin", "rb");
	      if(savegame != NULL){
		fclose(savegame);
		loadGame(savegame, thisRound, thisSnake, levelSettings);
		play(thisSnake, sair, levelSettings, thisRound, cenario, savegame);
	      }
	      else wprintw(aviso_win, "O JOGO NÃO PÔDE SER CARREGADO");
	      wrefresh(aviso_win);
	    }
	    else if(item_name(selec) == "CREDITOS"){
	      wclear(jogo_win);
	      mvwprintw(jogo_win, 4, 30, "DESENVOLVIDO POR:");
	      mvwprintw(jogo_win, 6, 20, "RICARDO G. HERDT E PAULO R. LANZARIN");
	      mvwprintw(jogo_win, 8, 18, "ALGORITMOS E PROGRAMAÇÃO, TURMA C. 2013/1");
	      mvwprintw(jogo_win, 10, 22, "UFRGS - INSTITUTO DE INFORMÁTICA");
	      wrefresh(jogo_win);
	      usleep(5000000);
	    }
	    else if(item_name(selec) == "SAIR"){ /* finaliza o jogo */
	      menuexit = 1;
	    }	      
	  }	
	}
    }
  unpost_menu(menu); /* tira o menu da tela */
  free(itens); /* libera memória usada por itens */
  free_menu(menu); /* libera memória usada pelo menu */
}
