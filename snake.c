#include <ncurses.h>
#include <stdlib.h>
#include <unistd.h> /* usleep */
#include <locale.h> /* para codificar em Unicode */
#include <wchar.h>
#include <string.h>
#define MAXTAM 95
#define MAXY 24
#define MAXX 80

WINDOW *jogo_win;
WINDOW *info_win;
WINDOW *save_game;

struct pos{
  int x;
  int y;
};

typedef struct pos pos;

struct levelSettings{
  int velocidade;
  int unidadesInc; /* unidades de incremento */
};

struct snakeData {
  struct pos cobra[MAXTAM]; /* array de pos, formando o corpo inteiro da cobra */
  struct pos posInc;
  int tam;
};

struct roundData {
  int passos;
  int alimCount;
  int nivel;
  struct pos alimentos[29];
};

typedef struct snakeData snakeData;

typedef struct roundData roundData;


void play(snakeData *thisSnake, int *sair, struct levelSettings *levelSettings, roundData *thisRound, FILE *cenario, FILE *savegame);

void storeGame(FILE *savegame, struct roundData *thisRound, struct snakeData *thisSnake, struct levelSettings *levelSettings);

void loadGame(FILE *savegame, struct roundData *thisRound, struct snakeData *thisSnake, struct levelSettings *levelSettings);

void setNivel(roundData *thisRound, FILE *cenario, struct levelSettings *levelSettings, snakeData *thisSnake);

void desenhaCenario(FILE *cenario, struct pos *alimentos);

void imprimeInfos(roundData *thisRound);

void initCobra(struct pos *cobra, int *tam);

void scanPos(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings, struct pos *alimentos, int *alimCount);

void morre(void);

void addAlimento(struct pos *alimentos, int *alimCount);

void desenhaCobra(struct pos *cobra, int *tam);

void incCobra(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings);

void input(snakeData *thisSnake, roundData *thisRound, FILE *cenario, struct levelSettings *levelSettings, int *sair, FILE *savegame);

void moveCobra(snakeData *thisSnake, struct levelSettings *levelSettings, roundData *thisRound);

		
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

  //system("resize -s 30 83"); // define o tamanho do terminal
  setlocale(LC_ALL,""); /* Unicode */
  initscr(); /* inicializa o modo curses */
  keypad(stdscr, TRUE); // possibilita o uso das setas
  // resize_term(30, 83); // permite com que o usuário mude o tamanho do terminal
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
  init_pair(1, COLOR_CYAN, COLOR_BLACK);
  init_pair(2, COLOR_RED, COLOR_BLACK);
  init_pair(3, COLOR_YELLOW, COLOR_BLACK);

  jogo_win = newwin(25, 81, 1, 1); /* janela do jogo */
  save_game = newwin(25, 81, 1, 1);
  info_win = newwin(2, 35, 27, 5); /* janela de informações */
  setNivel(&thisRound, cenario, &levelSettings, &thisSnake);
  play(&thisSnake, &sair, &levelSettings, &thisRound, cenario, savegame);
  refresh();
  wrefresh(jogo_win);
  endwin();
  return 0;
}

void play(snakeData *thisSnake, int *sair, struct levelSettings *levelSettings, roundData *thisRound, FILE *cenario, FILE *savegame)
{
  while(!(*sair))
    {
      timeout(0);
      if((thisRound->alimCount) >= 30) /* testa se o usuário já chegou ao objetivo */
	setNivel(thisRound, cenario, levelSettings, thisSnake); /* se já, muda nível */
      input(thisSnake, thisRound, cenario, levelSettings, sair, savegame);
      moveCobra(thisSnake, levelSettings, thisRound);
      imprimeInfos(thisRound);
      usleep(levelSettings->velocidade);
    }
}

/* SALVA OS SEGUINTES ATRIBUTOS EM jogo_salvo.bin:
   -thisRound  -levelSettings
   -tam   -posInc.x  -posInc.y  */
void storeGame(FILE *savegame, struct roundData *thisRound, struct snakeData *thisSnake, struct levelSettings *levelSettings)
{
  int i;
  if((savegame = fopen("jogo_salvo.bin", "w+b")) != NULL)
    {
      fwrite(levelSettings, sizeof(*levelSettings), 1, savegame); /* salva estrutura inteira */
      fwrite(thisRound, sizeof(*thisRound), 1, savegame); /* salva estrutura inteira */
      fwrite(&(thisSnake->tam), sizeof(int), 1, savegame); /* salva tamanho da cobra */
      fwrite(&(thisSnake->posInc.x), sizeof(int), 1, savegame); /* salva direção em que a cobra se movimentava */
      fwrite(&(thisSnake->posInc.y), sizeof(int), 1, savegame);
      for(i = 0; i < thisSnake->tam; i++) /* laço para salvar a posição da cobra */
	{
	  fwrite(&(thisSnake->cobra[i].x), sizeof(int), 1, savegame); 
	  fwrite(&(thisSnake->cobra[i].y), sizeof(int), 1, savegame);
	}
      putwin(jogo_win, savegame); /* salva o cenário/evita rebuilding do mesmo */
      fclose(savegame);
    }
  else 
    {
      wclear(info_win);
      wrefresh(info_win);
      wprintw(info_win, "O ARQUIVO NAO PODE SER ABERTO!");
    }
}

/* CARREGA OS SEGUINTES ATRIBUTOS DE jogo_salvo.bin:
   -thisRound  -levelSettings
   -tam   -posInc.x  -posInc.y  */
void loadGame(FILE *savegame, struct roundData *thisRound, struct snakeData *thisSnake, struct levelSettings *levelSettings)
{
  int i;
  wclear(jogo_win);
  if((savegame = fopen("jogo_salvo.bin", "r+b")) != NULL)
    {
      fread(levelSettings, sizeof(*levelSettings), 1, savegame); /* lê os settings do nível */
      fread(thisRound, sizeof(*thisRound), 1, savegame); /* lê as informações referentes ao estado do jogo */
      fread(&(thisSnake->tam), sizeof(int), 1, savegame); /* lê tamanho da cobra */
      fread(&(thisSnake->posInc.x), sizeof(int), 1, savegame); /* lê direção de movimentação da cobra */
      fread(&(thisSnake->posInc.y), sizeof(int), 1, savegame);
      for(i = 0; i < thisSnake->tam; i++) /* lê posição da cobra */
	{
	  fread(&(thisSnake->cobra[i].x), sizeof(int), 1, savegame);
	  fread(&(thisSnake->cobra[i].y), sizeof(int), 1, savegame);
	}
      jogo_win = getwin(savegame); /* restaura cenário/evita rebuilding do mesmo */
      //setNivel(&thisRound, cenario, &levelSettings, &thisSnake);
      fclose(savegame);
      wrefresh(jogo_win);
    }
  else 
    {
      wclear(info_win);
      wrefresh(info_win);
      wprintw(info_win, "O ARQUIVO NAO PODE SER ABERTO!");
    }
}

/* reseta todos os atributos da cobra, atualiza settings de nível (cenário, levelSettings), e inicia o jogo */
void setNivel(roundData *thisRound, FILE *cenario, struct levelSettings *levelSettings, snakeData *thisSnake)
{
  (thisRound->nivel)++; /* nivel: variável de teste do atual cenário */
  (thisRound->alimCount) = -1;
  (thisSnake->tam) = 4;
  thisSnake->posInc.x = 1;
  thisSnake->posInc.y = 0;
  wclear(jogo_win); /* limpa o cenário antigo */
  wrefresh(jogo_win);
  usleep(1000000); /* delay de entrada do novo cenário, colocarei mensagem "LEVEL 2/3" */
  if(thisRound->nivel == 1)
    {
      cenario = fopen("cenario1.txt", "r");
      levelSettings->velocidade = 100000;
      levelSettings->unidadesInc = 1;
    }
  else if(thisRound->nivel == 2)
    {
      cenario = fopen("cenario2.txt", "r"); /* carregamento do cenário é feito por aqui, não na função desenha */
      levelSettings->velocidade = 84500; /* settings do novo cenário */
      levelSettings->unidadesInc = 2; /* settings do novo cenário */
    }
  else if(thisRound->nivel == 3)
    {
      cenario = fopen("cenario3.txt", "r");
      levelSettings->velocidade = 69500;
      levelSettings->unidadesInc = 3;	
    }
  else morre();

  desenhaCenario(cenario, thisRound->alimentos); /* desenha novo cenário */
  addAlimento(thisRound->alimentos, &(thisRound->alimCount)); 
  initCobra(thisSnake->cobra, &(thisSnake->tam)); /* reinicia a cobra */
  wmove(jogo_win, thisSnake->cobra[0].y, thisSnake->cobra[0].x);
  desenhaCobra(thisSnake->cobra, &(thisSnake->tam));
  
}


void desenhaCenario(FILE *cenario, struct pos *alimentos)
{
  int i, alimInd, alimCount=0, maxY, maxX;
  int x, y, len;
  char obj, linha[20];
  
  getmaxyx(jogo_win,maxY,maxX);
  
  /* Bordas */

  wattron(jogo_win, COLOR_PAIR(3));
  for(i=0; i<maxX; i++)
    {
      mvwprintw(jogo_win, 0, i, "\u2588"); //superior
      mvwprintw(jogo_win, maxY-1, i, "\u2588"); //inferior
    }
  for(i=0; i<maxY; i++)
    {
      mvwprintw(jogo_win, i, 0, "\u2588"); //esquerda
      mvwprintw(jogo_win, i, maxX-1, "\u2588"); //direita
    }

  alimInd = 0; /* inicializa i para controlar adição de alimentos */

  /* Muros */
  while(fgets(linha, 80, cenario) != NULL)
    {
      if(linha[0] == 'H' || linha[0] == 'V')
	{
	  sscanf(linha, "%c %d %d %d ", &obj, &x, &y, &len);
	  if(obj == 'H') /* muro horizontal */
	    for(i=0; i<len; i++)
	      mvwprintw(jogo_win, y, x+i, "\u2588");
	  else if(obj == 'V') /* muro vertical */
	    for(i=0; i<len; i++)
	      mvwprintw(jogo_win, y+i, x, "\u2588");
	}
      else if
	(linha[0] == 'A') /* passa coordenadas de alimentos para o array */
	{
	  sscanf(linha, "%c %d %d ", &obj, &x, &y);
	  alimentos[alimInd].x = x;
	  alimentos[alimInd].y = y;
	  alimInd++;
	}
      else if
	(linha[0] == '#') /*ignora comentários. Desnecessário, mas por segurança...*/
	{}
    }
  wattroff(jogo_win, COLOR_PAIR(3));
  fclose(cenario);
  
  refresh();
  wrefresh(jogo_win);
}


void imprimeInfos(roundData *thisRound)
{
  int x, y;

  getyx(info_win, y, x);
  wmove(info_win, 0, 0);
  wprintw(info_win, "Tamanho da cobra: %d", ((thisRound->alimCount)*(thisRound->nivel)+4)); /* tamanho base + nivel */
  wprintw(info_win, "  Passos: %d", thisRound->passos); /* contador de passos */
  wprintw(info_win, "\nObjetivo: %d", 30*(thisRound->nivel)+4); /* tamanho da cobra almejado */
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

/* analisa elementos contidos na posição atual */
void scanPos(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings, struct pos *alimentos, int *alimCount)
{
  char elem;
  elem = (winch(jogo_win) & A_CHARTEXT);
  
  if(elem == -120 || elem == 35) /* Caractere bloco ou própria cobra */
    morre();
  else if (elem == 42) /* alimento */
    {
      incCobra(cobra, appendPos, tam, levelSettings);
      addAlimento(alimentos, alimCount);
    }
  wmove(jogo_win, cobra[0].y, cobra[0].x); /* restaura posição */
}

void morre(void)
{
  endwin();
  exit(1);
}

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
      elem = (inch() & A_CHARTEXT);
      if(elem == 32)
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

void desenhaCobra(struct pos *cobra, int *tam)
{
  int i, x, y;

  wattron(jogo_win, COLOR_PAIR(1)); /* habilita cor */
  waddch(jogo_win, 'Q'); /* cabeça */
  for(i=1; i<(*tam); i++) /* corpo */
    {
      mvwaddch(jogo_win, cobra[i].y, cobra[i].x, '#');
    }
  mvwaddch(jogo_win, cobra[(*tam)].y, cobra[(*tam)].x, ' '); /* fim da cobra com espaço para 'limpar o rastro' */
  wattroff(jogo_win, COLOR_PAIR(1));
  wrefresh(jogo_win);
}

void incCobra(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings)
{
  int diffX, diffY, i;
  for(i=0; i<levelSettings->unidadesInc; i++)
    (*tam)++;
  cobra[*tam].x = appendPos->x;
  cobra[*tam].y = appendPos->y;
  diffX = cobra[*tam - 1].x - cobra[*tam].x; /* diffX e diffY calculam o sentido de crescimento */
  diffY = cobra[*tam - 1].y - cobra[*tam].y;
  for(i=1; i < levelSettings->unidadesInc; i++)
    {
      cobra[*tam + i].x = appendPos->x - diffX; /* transmite as coordenadas para unidadesInc-1 */
      cobra[*tam + i].y = appendPos->y - diffY; /* após a última parte da cobra */
    }
}

void input(snakeData *thisSnake, roundData *thisRound, FILE *cenario, struct levelSettings *levelSettings, int *sair, FILE *savegame)
{
  int dir; // tipo int necessário para reconhecer setas
  dir = toupper(getch());
  //usleep(levelSettings->velocidade);
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
      *sair = 1;

    case 'T': /* trapaça */
      setNivel(thisRound, cenario, levelSettings, thisSnake);
      break;

    case 'G': /* salva jogo */
      storeGame(savegame, thisRound, thisSnake, levelSettings);
      break;

    case 'C': //int overwrite(const WINDOW *srcwin, WINDOW *dstwin)
      loadGame(savegame, thisRound, thisSnake, levelSettings);
      break;
    }
}

void moveCobra(snakeData *thisSnake, struct levelSettings *levelSettings, roundData *thisRound)
{
  int i;
  struct pos temp;
  int incFlag = 1;

  temp.x = thisSnake->cobra[thisSnake->tam].x;
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

  scanPos(thisSnake->cobra, &temp, &(thisSnake->tam), levelSettings, thisRound->alimentos, &(thisRound->alimCount));

  desenhaCobra(thisSnake->cobra, &(thisSnake->tam));
}
