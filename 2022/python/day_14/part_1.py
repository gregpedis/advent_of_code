import sys
sys.path.append(".")
import base as b
import os

class Wall:
    def __init__(self, left, right):
        parts1 = left.split(",")
        parts2 = right.split(",")
        self.x1 = int(parts1[0])
        self.y1 = int(parts1[1])
        self.x2 = int(parts2[0])
        self.y2 = int(parts2[1])
    
    def update(self, grid):
        if self.x1 == self.x2:
            line_from = min(self.y1, self.y2)
            line_to = max(self.y1, self.y2)+1
            for i in range(line_from, line_to):
                grid[i][self.x1] = "#"
        else:
            line_from = min(self.x1, self.x2)
            line_to = max(self.x1, self.x2)+1
            for i in range(line_from, line_to):
                grid[self.y1][i] = "#"


def get_walls(lines):
    walls = []
    for l in lines:
        parts = l.split("->")
        for i in range(len(parts)-1):
            left = parts[i]
            right = parts[i+1]
            walls.append(Wall(left,right))
    return walls

def get_minimum(walls):
    target_x = walls[0].x1
    for w in walls:
        target_x = min(target_x, w.x1, w.x2)
    return target_x

def get_maximums(walls):
    target_x = walls[0].x1
    target_y = walls[0].y1
    for w in walls:
        target_x = max(target_x, w.x1, w.x2)
        target_y = max(target_y, w.y1, w.y2)
    return (target_x, target_y)


def update_walls(walls, min_x):
    for w in walls:
        w.x1 -=min_x
        w.x2 -=min_x

def draw_grid(grid):
    os.system("clear")
    print("\n".join(["".join(x) for x in grid]))
    

def in_bounds(grid, x,y):
    if x < 0 or y < 0:
        return False
    if x >= len(grid) or y>= len(grid[0]):
        return False
    return True

def drop_one_sand(grid, start_pos):
    curr_row = -1
    curr_col = start_pos
    while True:
        if in_bounds(grid, curr_row+1, curr_col):
            if grid[curr_row+1][curr_col] == ".":
                curr_row +=1
            elif in_bounds(grid,curr_row+1, curr_col-1):
                if grid[curr_row+1][curr_col-1] == ".":
                    curr_row +=1
                    curr_col-=1
                elif in_bounds(grid, curr_row+1, curr_col+1):
                    if grid[curr_row+1][curr_col+1] == ".":
                        curr_row +=1
                        curr_col +=1
                    else:
                        grid[curr_row][curr_col] = "o"
                        return curr_row != -1 
                else:
                    return False
            else:
                return False
        else:
            return False


def drop_sand(grid, start_pos):
    sand_count = 0
    while True:
        if drop_one_sand(grid, start_pos):
            sand_count+=1
        else:
            return sand_count


# 478,51 -> 478,52 -> 491,52 -> 491,51
lines = b.get_input_first_lines("day_14")
walls = get_walls(lines)

min_x = get_minimum(walls)
update_walls(walls, min_x)
(max_x,max_y) = get_maximums(walls)
grid = [["."] * (max_x+1) for _ in range(max_y+1)]

for w in walls:
    w.update(grid)

draw_grid(grid)
sand_count = drop_sand(grid, 500-min_x)
draw_grid(grid)
print(sand_count)
