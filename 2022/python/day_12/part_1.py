import sys
sys.path.append(".")
import base as b
from collections import deque as dq

START = "S"
END = "E"
OFFSETS = [(0,1), (0,-1), (1,0), (-1,0)]

def in_bounds(grid, i,j):
    if i < 0 or j < 0:
        return False
    if i >= len(grid):
        return False
    if j >= len(grid[0]):
        return False
    return True


def can_move(point_from, point_to):
    # hacky but who cares?
    if point_from == "S":
        point_from = "a" 
    if point_from == "E":
        point_from = "z" 
    if point_to == "S":
        point_to = "a" 
    if point_to == "E":
        point_to = "z" 
    
    v_from = ord(point_from)
    v_to = ord(point_to)
    return v_from +1 >= v_to

def get_starting_position(grid):
    for i, row in enumerate(grid):
        for j, value in enumerate(row):
            if value == START:
                return (i,j)

def reached_end_position(grid, i, j):
    return grid[i][j] == END


def take_step(grid, x,y, steps):
    for ox,oy in OFFSETS:
        x2,y2 = x+ox, y+oy
        if in_bounds(grid,x2,y2):
            if reached_end_position(grid, x2,y2):
                yield len(steps)+1
            elif (x2,y2) not in steps and can_move(grid[x][y], grid[x2][y2]):
                new_steps = set(steps)
                new_steps.add((x2,y2))
                gen = take_step(grid, x2,y2, new_steps)
                for solution in gen:
                    yield solution


grid = b.get_input_first_lines("day_12")
(x,y) = get_starting_position(grid) 
steps = set()
steps.add((x,y))
solutions = next(steps for steps in take_step(grid,x,y, steps))

pass
