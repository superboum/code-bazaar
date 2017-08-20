#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define SUDOKU_NUMBER 50

typedef enum {FILL, SUCCESS, FAIL} state;

typedef struct {
  //grid[LINE][COL]
  int grid[10][10];
  state progress;
} sudoku;

typedef struct {
  int x;
  int y;
  int possibilities[9];
} coord;

/* COORDINATES */

int pos_length(int* t) {
  int i = 0, sum = 0;
  for (;i<9;i++) sum += t[i] == 0 ? 0 : 1;
  return sum;
}

void coord_to_string(coord *p) {
  int i;
  printf("x: %d, y: %d\npossibilities: ", p->x, p->y);
  for (i=0; i<9;i++) printf("%d ", p->possibilities[i]);
  printf("\ntotal possibilities : %d\n", pos_length(p->possibilities));
}

/* SUDOKU */

void sudoku_to_string(sudoku* s) {
  int x,y;
  for (x=0; x<9; x++) {
    for (y=0; y<9; y++) printf("%d ", s->grid[x][y]);
    printf("\n");
  }
}

void add_to_sudoku(sudoku* s, int line, char* content) {
  int col;
  for (col = 0; col < 9; col++) {
    s->grid[line][col] = (int)(content[col] - 48);
  }
}

void fetch(sudoku* db) {
  int matched_lines = 0, matched_items = 0;
  char buf[20], line[9];
  while (fgets (buf, sizeof(buf), stdin)) {
    matched_items = sscanf(buf, "%c%c%c%c%c%c%c%c%c", 
        &line[0], &line[1], &line[2], &line[3], &line[4], &line[5], &line[6], &line[7], &line[8]);
    if (matched_items == 9) {
      if (matched_lines % 9 == 0) db[matched_lines/9].progress = FILL;
      add_to_sudoku(&(db[matched_lines/9]), matched_lines % 9, line);
      matched_lines++;
    }
  }
}

int can_place_number_line(sudoku *s, int x, int y, int n) {
  int i=0;
  for(;i<9;i++)
    if(s->grid[x][i] == n) return 0;
  return 1;
}

int can_place_number_col(sudoku *s, int x, int y, int n) {
  int i=0;
  for(;i<9;i++)
    if(s->grid[i][y] == n) return 0;
  return 1;
}

int can_place_number_square(sudoku *s, int x, int y, int n) {
  int i,j;
  for(i=x/3*3;i<x/3*3+3;i++) {
    for(j=y/3*3;j<y/3*3+3;j++) {
      if(s->grid[i][j] == n) return 0;
    }
  }
  return 1;
}

void find_possibilities(int* ret, sudoku* s, int x, int y) {
  int i = 0; 
  for (;i<9;i++) { 
    ret[i] = can_place_number_col(s,x,y,ret[i]) && 
             can_place_number_line(s,x,y,ret[i]) &&
             can_place_number_square(s,x,y,ret[i]) ? ret[i] : 0;
    /*printf("x: %d, y:%d, n: %d, %s\n", x,y,i+1,ret[i]?"ok":"nop");*/
  }
}

coord create_cord(int x, int y, int* poss) {
  coord p = {x, y, {0}};
  memcpy((&p)->possibilities, poss, sizeof(int)*9 );
  return p;
}

coord best_box(sudoku *s) {
  coord p = {-1, -1, {1,2,3,4,5,6,7,8,9}};
  for (int x = 0; x < 9; x++) {
    for (int y = 0; y < 9; y++) {
      if (s->grid[x][y] == 0) {
        int possibilities[9] = {1,2,3,4,5,6,7,8,9};
        find_possibilities(possibilities, s, x, y);
        if (pos_length(p.possibilities) > pos_length(possibilities))
           p = create_cord(x, y, possibilities);
      }
    }
  }
  return p;
}

void solve(sudoku* s) {
  coord p = best_box(s);
  if (p.x == -1)
    s->progress = SUCCESS;
  else if (pos_length(p.possibilities) == 0)
    s->progress = FAIL;
  else {
    int i=0; sudoku ns;
    ns.progress = FILL;
    for(;i<9 && ns.progress != SUCCESS;i++) {
      if (p.possibilities[i] != 0) {
        memcpy(&ns, s, sizeof(sudoku));
        ns.grid[p.x][p.y] = p.possibilities[i];
        solve(&ns);
        if(ns.progress == SUCCESS) {
          memcpy(s, &ns, sizeof(sudoku));
        }
      }
    }
    if (s->progress != SUCCESS) s->progress = FAIL;
  }
}

void euler_solve(sudoku* s, int* acc) {
  *acc += s->grid[0][0]*100+s->grid[0][1]*10+s->grid[0][2];
}

int main(void) {
  sudoku db[SUDOKU_NUMBER];
  fetch(db);
  int n=0, euler_answer=0;
  for(;n<50;n++) {
    printf("--- GRID %d ---\n", n+1);
    solve(&db[n]);
    euler_solve(&db[n],&euler_answer);
    sudoku_to_string(&db[n]);
  }
  printf("Euleur: %d\n", euler_answer);
  return 0;
}
