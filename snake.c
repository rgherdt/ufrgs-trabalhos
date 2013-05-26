#include <ncurses.h>
#include <stdlib.h>
#include <unistd.h> /* usleep */
#include <locale.h> /* para codificar em Unicode */
#include <wchar.h>
#include <string.h>
#define MAXTAM 20
#define MAXY 24
#define MAXX 80

struct pos;

struct pos{
  int x;
  int y;
};

struct levelSettings{
  int velocidade;
  int unidadesInc; /* unidades de incremento */
};

void initCobra(struct pos *cobra, int *tam);

void desenhaCobra(struct pos *cobra, int *tam);

void incCobra(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings);

void input(struct pos *posInc, int *sair);

void moveCobra(struct pos *cobra, struct pos *posInc, int *tam, struct levelSettings *levelSettings, struct pos *alimentos, int *alimCount);

void desenhaCenario(FILE *cenario, struct pos *alimentos);

void addAlimento(struct pos *alimentos, int *alimCount);

void scanPos(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings, struct pos *alimentos, int *alimCount);

void morre(void);

void play(struct pos *cobra, struct pos *posInc, int *tam, int *sair, struct levelSettings *levelSettings, struct pos *alimentos, int *alimCount);

void imprimeInfos(int *alimCount);
 
int main(int argc, char *argv[])
{
  int i, sair=0;
  int tam = 4;
  struct pos cobra[MAXTAM]; /* array de pos, formando o corpo inteiro da cobra */
  struct pos posInc;
  struct levelSettings levelSettings;
  struct pos alimentos[29];
  int alimCount=-1;
  FILE *cenario;

  setlocale(LC_ALL,""); /* Unicode */
  initscr(); /* inicializa o modo curses */
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

  levelSettings.velocidade = 10000;
  levelSettings.unidadesInc = 1;
  posInc.x = 1;
  posInc.y = 0; /* cobra inicia movendo-se para a direita */

  desenhaCenario(cenario, alimentos);
  addAlimento(alimentos, &alimCount);
  initCobra(cobra, &tam);
  move(cobra[0].y, cobra[0].x);
  desenhaCobra(cobra, &tam);
  
  play(cobra, &posInc, &tam, &sair, &levelSettings, alimentos, &alimCount);
  
  endwin();
  return 0;
}

void play(struct pos *cobra, struct pos *posInc, int *tam, int *sair, struct levelSettings *levelSettings, struct pos *alimentos, int *alimCount)
{
  while(!(*sair))
    {
      timeout(0);
      input(posInc, sair);
      moveCobra(cobra, posInc, tam, levelSettings, alimentos, alimCount);
      imprimeInfos(alimCount);
      usleep(100000);
    }
}

void desenhaCenario(FILE *cenario, struct pos *alimentos)
{
  int i, alimInd, alimCount=0;
  int x, y, len;
  char obj, linha[20];

  //  getmaxyx(stdscr,maxY,maxX);
  
  /* Bordas */
  attron(COLOR_PAIR(3));
  for(i=0; i<=MAXX; i++)
    {
      mvprintw(0, i, "\u2588");
      mvprintw(MAXY, i, "\u2588");
    }
  for(i=0; i<MAXY; i++)
    {
      mvprintw(i, 0, "\u2588");
      mvprintw(i, MAXX, "\u2588");
    }

  cenario = fopen("cenario1.txt", "r");
  alimInd = 0; /* inicializa i para controlar adição de alimentos */

  /* Muros */
  while(fgets(linha, 20, cenario) != NULL)
    {
      if(linha[0] == 'H' || linha[0] == 'V')
	{
	  sscanf(linha, "%c %d %d %d ", &obj, &x, &y, &len);
	  if(obj == 'H') /* muro horizontal */
	    for(i=0; i<len; i++)
	      mvprintw(y, x+i, "\u2588");
	  else if(obj == 'V') /* muro vertical */
	    for(i=0; i<len; i++)
	      mvprintw(y+i, x, "\u2588");
	}
      else if
	(linha[0] == 'A') /* passa coordenadas de alimentos para o array */
	{
	  sscanf(linha, "%c %d %d ", &obj, &x, &y);
	  alimentos[alimInd].x = x;
	  alimentos[alimInd].y = y;
	  alimInd++;
	}
    }
  attroff(COLOR_PAIR(3));
  fclose(cenario);
  //  mvprintw(10, 10, "y: %d x: %d i: %d\n", y, x, alimInd);

  //  addAlimento(alimentos[1]);
  //  addAlimento(alimentos[2]);

  refresh();
}

void imprimeInfos(int *alimCount)
{
  int x, y;
  getyx(stdscr, y, x);
  move(27, 5);
  printw("Pontos: %d", *alimCount);
  move(y, x); 
}

/* inicializa a estrutura da cobra povoando o array cobra com as coordenadas */
/* centrais da tela (voltada horizontalmente para a direita).                */
/* Adiciona-se mais um elemento ao fim da cobra, o qual será utilizado para  */
/* finalizá-la com o caractere ' ' na função desenhaCobra                    */
void initCobra(struct pos *cobra, int *tam) 
{
  int i;
  //  getmaxyx(stdscr,maxY,maxX);

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
  elem = (inch() & A_CHARTEXT);
  
  if(elem == -120 || elem == 35) /* Caractere bloco ou própria cobra */
    morre();
  else if (elem == 42) /* alimento */
    {
      incCobra(cobra, appendPos, tam, levelSettings);    
      addAlimento(alimentos, alimCount);
    }
  move(cobra[0].y, cobra[0].x); /* restaura posição */
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
      move(y, x);
      elem = (inch() & A_CHARTEXT);
      if(elem == 32)
	{
	  attron(COLOR_PAIR(2)); /* maças vermelhas */
	  addch('*');
	  attroff(COLOR_PAIR(2));
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

  refresh();
}

void desenhaCobra(struct pos *cobra, int *tam)
{
  int i, x, y;

  attron(COLOR_PAIR(1)); /* habilita cor */
  addch('Q'); /* cabeça */
  for(i=1; i<(*tam); i++) /* corpo */
    {
      mvaddch(cobra[i].y, cobra[i].x, '#');
    }
  mvaddch(cobra[(*tam)].y, cobra[(*tam)].x, ' '); /* fim da cobra */
  attroff(COLOR_PAIR(1));
  refresh();
}

void incCobra(struct pos *cobra, struct pos *appendPos, int *tam, struct levelSettings *levelSettings)
{
  int diffX, diffY, i;
  for(i=0; i<levelSettings->unidadesInc; i++)
    (*tam)++;
  cobra[*tam].x = appendPos->x;
  cobra[*tam].y = appendPos->y;
  diffX = cobra[*tam - 1].x - cobra[*tam].x;
  diffY = cobra[*tam - 1].y - cobra[*tam].y;
  for(i=1; i < levelSettings->unidadesInc; i++)
    {
      cobra[*tam + i].x = appendPos->x - diffX;
      cobra[*tam + i].y = appendPos->y - diffY;
    }
}

void input(struct pos *posInc, int *sair)
{
  char dir;
  dir = toupper(getch());

  switch(dir)
    {
    /* Ordenadas iniciam no topo superior da tela */
    case 'D':
      if(posInc->x != -1)
	{
	  posInc->x = 1;
	  posInc->y = 0;
	}
      break;
    case 'A':
      if(posInc->x != 1)
	{
	  posInc->x = -1;
	  posInc->y = 0;
	}
      break;
    case 'W':
      if(posInc->y != 1)
	{
	  posInc->x = 0;
	  posInc->y = -1; 
	}
      break;
    case 'X':
      if(posInc->y != -1)
	{
	  posInc->x = 0;
	  posInc->y = 1;
	}
      break;
    case 'Q':
      *sair = 1;
    }
}

void moveCobra(struct pos *cobra, struct pos *posInc, int *tam, struct levelSettings *levelSettings, struct pos *alimentos, int *alimCount)
{
  int i;
  struct pos temp;
  int incFlag = 1;

  /* if(incFlag) */
  /*   { */
  /*     //      *tam++; */
  /*     cobra[*tam+1].x = cobra[*tam].x; */
  /*     cobra[*tam+1].y = cobra[*tam].y; */
  /*   } */
    
  temp.x = cobra[*tam].x;
  temp.y = cobra[*tam].y;
  /* transfere as coordenadas dos elos de trás pra frente */
  for(i=(*tam); i>0; i--)
    {
      cobra[i].x = cobra[i-1].x;
      cobra[i].y = cobra[i-1].y;
    }

  /* temp.x = cobra[0].x; */
  /* temp.y = cobra[0].y; */
  cobra[0].x += posInc->x; /* calcula a posição da cabeça */
  cobra[0].y += posInc->y;  
  move(cobra[0].y, cobra[0].x);

  scanPos(cobra, &temp, tam, levelSettings, alimentos, alimCount);

  desenhaCobra(cobra, tam);
}

