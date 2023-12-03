from math import prod

# PART 1

def get_numbers(grid):
  numbers = []
  idx = None
  acc = ""
  for x in range(len(grid)):
    for y in range(len(grid[0])):
      curr = grid[x][y]
      if curr.isdigit():
        acc += curr
        if idx is None:
          idx = (x,y)
      elif acc != "":
        numbers.append((idx, acc))
        idx = None
        acc = ""
    if acc != "":
        numbers.append((idx, acc))
        idx = None
        acc = ""
  return numbers

def get_part_numbers(grid, numbers):
  res = []
  for ((x,y), number) in numbers:
    for offset in range(len(number)):
      if check_location(grid, x, y+offset):
        res.append(int(number))
        break
  return res

def check_location(grid, x,y):
  locations = get_locations(grid, x, y)
  for (x1,y1) in locations:
    target = grid[x1][y1]
    if not target.isdigit() and target != ".":
      return True
  return False

# PART_2
def get_gears(grid):
  gears = []
  for x in range(len(grid)):
    for y in range(len(grid[0])):
      curr = grid[x][y]
      if curr == "*":
        gears.append((x,y))
  return gears

def get_gear_numbers(grid, gears):
  res = [] # [(n1,n2), (n3,n4)]
  for (x,y) in gears:
    locations = get_gear_adjacent_locations(grid, x, y) # [(x1,y2), (x2,y2),...]
    gear_ratio = get_gear_ratio(grid, locations)
    res.append(gear_ratio)
  return res

def get_gear_adjacent_locations(grid, x, y):
  res = []
  locations = get_locations(grid,x,y)
  for (x1,y2) in locations:
    target = grid[x1][y2]
    if target.isdigit():
      res.append((x1,y2))
  return res

def get_gear_ratio(grid, locations):
  numbers = set()
  for (x,y) in locations:
    number = grid[x][y]
    currY = y+1
    while currY < len(grid[0]):
      if grid[x][currY].isdigit():
        number = number + grid[x][currY]
        currY+=1
      else:
        break
    currY = y-1
    while currY >= 0:
      if grid[x][currY].isdigit():
        number = grid[x][currY] + number
        currY-=1
      else:
        break
    numbers.add((x,currY, number))
  if len(numbers) == 2:
    return prod([int(number) for (_,_,number) in numbers])
  else:
    return 0

# COMMON

def get_grid():
  with open("day_3.txt", "rt") as f:
      return [x.replace("\n", "") for x in f.readlines()]

def get_locations(grid, x, y):
  locations = []
  if y > 0:
    locations.append((x,y-1))
    if x > 0:
      locations.append((x-1,y-1))
    if x < len(grid)-1:
      locations.append((x+1, y-1))
  if x>0:
    locations.append((x-1, y))
  if x< len(grid)-1:
    locations.append((x+1, y))
  if y < len(grid[0])-1:
    locations.append((x,y+1))
    if x > 0:
      locations.append((x-1, y+1))
    if x<len(grid)-1:
      locations.append((x+1, y+1))
  return locations

if __name__ == "__main__":
  grid = get_grid()
# part 1
  numbers = get_numbers(grid)
  valid_numbers = get_part_numbers(grid, numbers)
  res1 = sum(valid_numbers)
  print(f"Res1: {res1}")
# part 2
  gears = get_gears(grid)
  res2= sum(get_gear_numbers(grid, gears))
  print(f"Res2: {res2}")