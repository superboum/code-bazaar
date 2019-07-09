#!/usr/bin/python3

def empty_grid():
  return [
    [None, None, None],
    [None, None, None],
    [None, None, None]
  ]

def is_win_line(grid):
  for line in grid:
    if line[0] == line[1] == line[2] != None:
      return line[0]
  return None

def is_win_col(grid):
  for i in range(3):
    if grid[0][i] == grid[1][i] == grid[2][i] != None:
      return grid[0][i]
    return None

def is_win_diag(grid):
  if grid[0][0] == grid[1][1] == grid[2][2] != None:
    return grid[0][0]
  if grid[0][2] == grid[1][1] == grid[2][0] != None:
    return grid[0][2]
  return None

def is_draw(grid):
  for line in grid:
    for box in line:
      if box == None: return None
  return "draw"

def game_status(grid):
  return is_win_line(grid) or \
  is_win_col(grid) or \
  is_win_diag(grid) or \
  is_draw(grid) or \
  "in_progress"

def all_possibilities(grid, current_player, next_player):
  status = game_status(grid)
  children = empty_grid()

  if status == "in_progress":
    for i in range(3):
      for j in range(3):
        if grid[i][j] != None: continue

        grid[i][j] = current_player
        children[i][j] = all_possibilities(grid, next_player, current_player)
        grid[i][j] = None

        comp_status, _ = children[i][j]
        if (comp_status or status) == current_player:
          status = current_player
        elif (comp_status or status) == "draw":
          status = "draw"
        else:
          status = next_player
        
      
  return (status, children)

def print_grid(grid):
  cara = {None: "_", "cross": "X", "circle": "O"}
  print("Grid:")
  for line in grid:
    print(f"    {cara[line[0]]} {cara[line[1]]} {cara[line[2]]}")
  print()

def get_input():
  prompt = "Enter line and column separated by space (ex: 0 2): "
  return [max(0, min(2, int(x))) for x in input(prompt).split()]

def check_game(grid):
  print_grid(grid)
  cur_status = game_status(grid)
  print(f"Winner: {cur_status}")
  if cur_status != "in_progress":
    return True
  return False

def human_player(grid, unroll_view, player):
  while True:
    line, col = get_input()
    if grid[line][col] != None: continue
    grid[line][col] = player
    _, unroll_view = unroll_view[line][col]
    return grid, unroll_view

def computer_player(grid, unroll_view, player, other_player):
  best = 0
  coord = None
  prio = {None: 0, other_player: 1, "draw": 2, player: 3}

  for i in range(3):
    for j in range(3):
      if unroll_view[i][j] == None: continue
      candidate, _ = unroll_view[i][j]
      if best < prio[candidate]:
        coord = (i, j)
        best = prio[candidate]
  line, col = coord
  grid[line][col] = player
  _, unroll_view = unroll_view[line][col]
  print(f"computer plays {line} {col}")
  return grid, unroll_view

human_symbol, computer_symbol = "cross", "circle"
winner, unroll = all_possibilities(empty_grid(), "cross", "circle")

board = empty_grid()
unroll_view = unroll

if "c" in input("Who starts? (c)omputer or (h)uman?"):
  computer_symbol, human_symbol = human_symbol, computer_symbol
  board, unroll_view = computer_player(board, unroll_view, computer_symbol, human_symbol)

while True:
  if check_game(board): break
  board, unroll_view = human_player(board, unroll_view, human_symbol)

  if check_game(board): break
  board, unroll_view = computer_player(board, unroll_view, computer_symbol, human_symbol)
  
      
        

