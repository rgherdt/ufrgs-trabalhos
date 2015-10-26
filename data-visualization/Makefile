CC     = gcc
EXTRAFLAGS = -O3 -Wall -g -export-dynamic
UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S), Darwin)
	LIBS   =  -lm -framework GLUT -framework OpenGL
endif
ifeq ($(UNAME_S), Linux)
	LIBS   =  -lm -lglut -lGLU -lGL
endif

data-visualization : main.c data-serializate.o error.o coordinates.o
	$(CC) $(EXTRAFLAGS) $(LIBS) -o data-visualization main.c data-serializate.o error.o coordinates.o

data-serializate.o : data-serializate.c
	$(CC) $(EXTRAFLAGS) $(LIBS) -c data-serializate.c

coordinates.o : coordinates.c
	$(CC) $(EXTRAFLAGS) $(LIBS) -c coordinates.c

error.o : error.c
	$(CC) $(EXTRAFLAGS) $(LIBS) -c error.c

.PHONY: clean
clean:
	rm -f *.o data-visualization

