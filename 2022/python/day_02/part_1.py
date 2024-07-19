import sys
sys.path.append(".")
import base as b


converter = {
    "A" : "X", # Rock
    "B" : "Y", # Paper
    "C" : "Z"  # Scizzors
}

points = {
    "X" : 1, # Rock
    "Y" : 2, # Paper
    "Z" : 3, # Scizzors
}

beaten_by = {
    "X": "Y", # Rock beaten by Paper
    "Y": "Z", # Paper beaten by Scizzors
    "Z": "X" # Scizzors beaten by Rock
}

def get_pairs(lines):
    pairs = []
    for line in lines:
        parts = line.split(" ")
        pairs.append((converter[parts[0]], parts[1]))
    return pairs


def get_points_for_result(me, other):
    if me == other:
        return 3
    if me == beaten_by[other]:
        return 6
    return 0

def get_points_for_choice(me):
    return points[me]



lines = b.get_input_first_lines("day_02")
pairs = get_pairs(lines)
points_per_round = [get_points_for_choice(me)+get_points_for_result(me,other) for other,me in pairs]
print(sum(points_per_round))
