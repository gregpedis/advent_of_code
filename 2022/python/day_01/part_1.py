import sys
sys.path.append(".")
import base as b


lines = b.get_input_first_lines("day_01")

elves = []
curr = 0
for line in lines:
    if not line:
        elves.append(curr)
        curr = 0
    else:
        curr+=int(line)

print(max(elves))
    