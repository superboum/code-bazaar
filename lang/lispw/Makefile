CC = gcc
CFLAGS = -Wall -Wextra -O2  -fsanitize=address -g 
LDFLAGS = -rdynamic  -fsanitize=address

BIN_DIR = bin
SRC_DIR = src
OBJECTS = $(BIN_DIR)/mem.o \
            $(BIN_DIR)/runtime.o

.PHONY: all clean
all: $(BIN_DIR)/repl
clean:
	@rm -rf $(BIN_DIR)

# -- COMPILE RUNTIME --
$(BIN_DIR)/mem.o: $(SRC_DIR)/runtime.h $(SRC_DIR)/mem.c
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/mem.c -o $(BIN_DIR)/mem.o

$(BIN_DIR)/runtime.o: $(SRC_DIR)/runtime.h $(SRC_DIR)/runtime.c
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/runtime.c -o $(BIN_DIR)/runtime.o

# -- COMPILE ENTRYPOINTS
$(BIN_DIR)/repl.o: $(SRC_DIR)/runtime.h $(SRC_DIR)/repl.c
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/repl.c -o $(BIN_DIR)/repl.o

$(BIN_DIR)/test.o: $(SRC_DIR)/runtime.h $(SRC_DIR)/test.c
	@mkdir -p $(BIN_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/test.c -o $(BIN_DIR)/test.o

# -- LINK --
$(BIN_DIR)/repl: $(OBJECTS) $(BIN_DIR)/repl.o
	@mkdir -p $(BIN_DIR)
	$(CC) $(LDFLAGS) $(OBJECTS) $(BIN_DIR)/repl.o -o $(BIN_DIR)/repl

$(BIN_DIR)/test: $(OBJECTS) $(BIN_DIR)/test.o
	@mkdir -p $(BIN_DIR)
	$(CC) $(LDFLAGS) $(OBJECTS) $(BIN_DIR)/test.o -o $(BIN_DIR)/test


