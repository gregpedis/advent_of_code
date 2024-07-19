import sys
sys.path.append(".")
import base as b

points_per_result = {
    "X" : 0, # Lose
    "Y" : 3, # Draw
    "Z" : 6, # Win
}

points_per_element = {
    "A" : 1, # Rock
    "B": 2, # Paper
    "C": 3, # Scizzors
}


should_play = {
    ("A","X"): "C",
    ("A","Y"): "A",
    ("A","Z"): "B",
    ("B","X"): "A",
    ("B","Y"): "B",
    ("B","Z"): "C",
    ("C","X"): "B",
    ("C","Y"): "C",
    ("C","Z"): "A",
}

def get_pairs(lines):
    pairs = []
    for line in lines:
        parts = line.split(" ")
        pairs.append((parts[0], parts[1]))
    return pairs


def get_points_for_result(res):
    return points_per_result[res]

def get_points_for_choice(other,res):
    me = should_play[(other,res)]
    return points_per_element[me]



lines = b.get_input_first_lines("day_02")
pairs = get_pairs(lines)
points_per_round = [get_points_for_choice(other,res)+get_points_for_result(res) for other,res in pairs]
print(sum(points_per_round))
