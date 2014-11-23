IDIR   = .
CC     = gcc
CFLAGS = -m32
ODIR   = obj
SRC    = src
BUILD  = build

DEPS = $(IDIR)/*.h

_OBJ = Graph.o AdjList.o MinHeap.o
OBJ = $(patsubst %,$(ODIR)/%,$(_OBJ))

$(ODIR)/%.o : $(SRC)/%.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c -o $@ $<

$(BUILD)/dk_adjlist_bin_heap: $(OBJ)
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) $(SRC)/main.c -o $@ $^

.PHONY: clean

clean:
	rm -f $(ODIR)/*.o $(BUILD)/*



