import sys
sys.path.append(".")
import base as b


class Area:
    def __init__(self, area_range: str):
        splitted = area_range.split("-")
        self.x1 = int(splitted[0])
        self.x2 = int(splitted[1])

    def contains_or_contained(self, other):
        if self.x1 <= other.x1 and self.x2 >= other.x2:
            return True
        elif other.x1 <= self.x1 and other.x2 >= self.x2:
            return True
        return False


def get_area_pairs(lines):
    pairs = (l.split(",") for l in lines)
    area_pairs = [(Area(p[0]), Area(p[1])) for p in pairs]
    return area_pairs


lines = b.get_input_first_lines("day_04")
area_pairs = get_area_pairs(lines)
filtered = [(l,r) for l,r in area_pairs if l.contains_or_contained(r)]
print(len(filtered))
