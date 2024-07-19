import sys
sys.path.append(".")
import base as b



lines = b.get_input_first_lines("day_08")
height = len(lines)
width = len(lines[0])

def check(curr, rest):
    for tree in rest:
        if tree >= curr:
            return False
    return True

def check_visibility(row_id, column_id):
    tree = lines[row_id][column_id]

    left = (lines[row_id][i] for i in range(0, column_id))
    right = (lines[row_id][i] for i in range(column_id+1, width))
    up = (lines[i][column_id] for i in range(0, row_id))
    down = (lines[i][column_id] for i in range(row_id+1, height))

    return check(tree, left) or check(tree, right) or check(tree, up) or check(tree, down)


visible = 0
for row_id, line in enumerate(lines):
    for column_id,tree in enumerate(line):
        if row_id == 0 or column_id == 0 or row_id == height-1 or column_id == width-1:
            visible+=1
        else:
            if check_visibility(row_id, column_id):
                visible+=1

print(visible)