import sys
sys.path.append(".")
import base as b
from collections import deque as dq

class Move:
    def __init__(self, line):
        parts = line.split(" ")
        self.how_many = int(parts[1])
        self.start = int(parts[3])
        self.end = int(parts[5])

class Stack:
    def __init__(self, column):
        self.stack = dq()
        self.id = int(column[-1])
        for value in column[:-1]:
            if value.strip():
                self.stack.appendleft(value)
        
    def __hash__(self):
        return self.id


def partition(line):
    cargos = []
    for i in range(0, len(line), 4):
        cargo = line[i:i+3]
        cargos.append(cargo)
    return cargos 

def transpose(data):
    transposed = [[data[j][i] for j in range(len(data))] for i in range(len(data[0]))]
    return transposed

def get_stacks(lines):
    partitioned = [partition(l) for l in lines]
    transposed = transpose(partitioned)
    stacks = [Stack(column) for column in transposed]
    return stacks


def split_lines(lines):
    midpoint = next(i for i,l in enumerate(lines) if not l)
    stacks = {s.id:s for s in  get_stacks(lines[:midpoint])}
    moves = [Move(l) for l in lines[midpoint+1:]]
    return (stacks, moves)


def apply_moves(stacks, moves):
    for m in moves:
        stack_from = stacks[m.start]
        stack_to = stacks[m.end]
        buffer = dq()
        for _ in range(m.how_many):
            cargo = stack_from.stack.pop()
            buffer.append(cargo)
        for _ in range(m.how_many):
            cargo = buffer.pop()
            stack_to.stack.append(cargo)
    
def print_top_blocks(stacks):
    top_blocks = ""
    for i in range(1, len(stacks)+1):
        top_blocks += stacks[i].stack.pop()
    print(top_blocks.replace("[","").replace("]",""))

lines = b.get_input_first_lines("day_05")
(stacks,moves) = split_lines(lines)
apply_moves(stacks,moves)
print_top_blocks(stacks)
