import sys
sys.path.append(".")
import base as b

OFFSET_LOWER = 96
OFFSET_UPPER = 38

def get_priority(item_type):
    offset = OFFSET_LOWER if item_type.islower() else OFFSET_UPPER
    return ord(item_type) - offset

def get_mistakes(bags):
    mistakes = []
    for bag in bags:
        midpoint = int(len(bag)/2)
        left = set(bag[:midpoint])
        right = set(bag[midpoint:])
        mistakes.extend(left.intersection(right))
    return mistakes

bags = b.get_input_first_lines("day_03")
mistakes = get_mistakes(bags)
priorities = map(get_priority, mistakes)
print(sum(priorities))
