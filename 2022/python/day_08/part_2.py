import sys
sys.path.append(".")
import base as b



lines = b.get_input_first_lines("day_08")
height = len(lines)
width = len(lines[0])


def score(curr, rest):
    total = 0
    curr = int(curr)
    for tree in rest:
        total+=1
        if int(tree) >= curr:
            return total
    return total

def total_score(row_id, column_id):
    tree = lines[row_id][column_id]

    left = (lines[row_id][i] for i in range(column_id-1, -1,-1))
    right = (lines[row_id][i] for i in range(column_id+1, width))
    up = (lines[i][column_id] for i in range(row_id-1,-1,-1))
    down = (lines[i][column_id] for i in range(row_id+1, height))

    return score(tree,left) * score(tree,right) * score(tree,up) * score(tree,down) 


optimal = 0
for row_id, line in enumerate(lines):
    for column_id,tree in enumerate(line):
        optimal = max(optimal, total_score(row_id, column_id))

print(optimal)