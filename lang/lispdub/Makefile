CC = gcc
CFLAGS = -Wall -Wextra -g 
LDFLAGS =

BIN_DIR = bin
SRC_DIR = src

.PHONY: all clean
all: $(BIN_DIR)/repl
clean:
	@rm -rf $(BIN_DIR)

$(BIN_DIR):
	@mkdir -p $(BIN_DIR)

# -- COMPILE --
$(BIN_DIR)/runtime.o: $(SRC_DIR)/runtime.h $(SRC_DIR)/runtime.c $(BIN_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/runtime.c -o $(BIN_DIR)/runtime.o

$(BIN_DIR)/repl.o: $(SRC_DIR)/repl.c $(BIN_DIR)
	$(CC) $(CFLAGS) -c $(SRC_DIR)/repl.c -o $(BIN_DIR)/repl.o

# -- LINK --
$(BIN_DIR)/repl: $(OBJECTS) $(SRC_DIR)/repl.c $(BIN_DIR)
	$(CC) $(LDFLAGS) $(BIN_DIR)/runtime.o $(BIN_DIR)/repl.o -o $(BIN_DIR)/repl
  
