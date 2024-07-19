import sys
sys.path.append(".")
import base as b

OFFSET_LOWER = 96
OFFSET_UPPER = 38

def get_priority(item_type):
    offset = OFFSET_LOWER if item_type.islower() else OFFSET_UPPER
    return ord(item_type) - offset

def get_intersection(elves):
    inter = set(elves[0])
    inter = inter.intersection(set(elves[1]))
    inter = inter.intersection(set(elves[2]))
    return inter

def get_badges(bags):
    badges = []
    for i in range(0, len(bags), 3):
        group = bags[i:i+3]
        badge = get_intersection(group)
        badges.extend(badge)
    return badges

bags = b.get_input_first_lines("day_03")
badges = get_badges(bags)
priorities = map(get_priority, badges)
print(sum(priorities))
