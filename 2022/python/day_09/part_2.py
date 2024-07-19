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
    

def calculate_new_position(head_x,head_y, tail_x,tail_y):
    diff_x = abs(head_x - tail_x)
    diff_y = abs(head_y - tail_y)
    mul_x = -1 if head_x < tail_x else 1
    mul_y = -1 if head_y < tail_y else 1

    if diff_x+diff_y > 2:
        return (tail_x + mul_x, tail_y + mul_y)
    elif diff_x > 1:
        return (tail_x + mul_x, tail_y)
    elif diff_y > 1:
        return (tail_x, tail_y + mul_y)
    return (tail_x, tail_y)


def apply_instructions(instructions):
    positions = [(0,0) for _ in range(10)] # all the tails
    tail_unique = set()
    for dir, steps in instructions:
        for _ in range(steps):
            positions[0] = get_head_position(positions[0][0], positions[0][1], dir)
            for idx in range(1,10):
                positions[idx] = calculate_new_position(positions[idx-1][0], positions[idx-1][1], positions[idx][0], positions[idx][1])
            tail_unique.add(positions[-1])
    return len(tail_unique)



lines = b.get_input_first_lines("day_09")
instructions = get_instructions(lines)
res = apply_instructions(instructions)


print(res)