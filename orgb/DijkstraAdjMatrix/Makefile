IDIR   = .
CC     = gcc
CFLAGS = -m32
ODIR   = obj
SRC    = src
BUILD  = build

_DEPS = Graph.h AdjList.h
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

_OBJ = Graph.o AdjList.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o : $(SRC)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BUILD)/dk_adjmatrix: $(OBJ)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $(SRC)/main.c -o $@ $^

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(BUILD)/*



