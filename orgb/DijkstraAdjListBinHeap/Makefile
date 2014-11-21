IDIR   = .
CC     = gcc
ODIR   = obj
SRC    = src
BUILD  = build

DEPS = $(IDIR)/*.h

_OBJ = Graph.o AdjList.o MinHeap.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o : $(SRC)/%.c
	@mkdir -p $(@D)
	$(CC) -c -o $@ $<

$(BUILD)/dk_adjlist_arrlist: $(OBJ)
	@mkdir -p $(@D)
	$(CC) $(SRC)/main.c -o $@ $^

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(BUILD)/*



