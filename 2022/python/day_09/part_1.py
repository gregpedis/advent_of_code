import sys
sys.path.append(".")
import base as b


def get_instructions(lines):
    instructions = []
    for l in lines:
        parts = l.split(" ")
        direction, amount = parts[0], int(parts[1])
        instructions.append((direction, amount))
    return instructions


def get_head_position(head_x, head_y, dir):
    if dir == "R":
        return (head_x+1, head_y)

    if dir == "L":
        return (head_x-1, head_y)

    if dir == "U":
        return (head_x, head_y+1)

    if dir == "D":
        return (head_x, head_y-1)
    
def is_far(x1,y1,x2,y2):
    diff_x = abs(x1-x2)
    diff_y = abs(y1-y2)
    if diff_x and diff_y:
        return diff_x + diff_y > 2
    else:
        return diff_x + diff_y > 1


def apply_instructions(instructions):
    positions = set()
    head_x, head_y = 0,0
    tail_x,tail_y = 0,0
    for dir, steps in instructions:
        for _ in range(steps):
            head_prev_x, head_prev_y = head_x, head_y
            head_x, head_y = get_head_position(head_x, head_y, dir)
            if is_far(head_x, head_y, tail_x, tail_y):
                tail_x = head_prev_x
                tail_y = head_prev_y
                positions.add((tail_x,tail_y))
    return len(positions)+1



lines = b.get_input_first_lines("day_09")
instructions = get_instructions(lines)
res = apply_instructions(instructions)


print(res)