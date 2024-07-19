import sys
sys.path.append(".")
import base as b
from collections import deque as dq


SEQUENCE_COUNT = 14

data = b.get_input_first_raw("day_06")
queue = dq()

for c in data[:SEQUENCE_COUNT-1]:
    queue.appendleft(c)

for index, c in enumerate(data[SEQUENCE_COUNT-1:]):
    queue.appendleft(c)
    if len(set(queue)) == SEQUENCE_COUNT:
        print(index+SEQUENCE_COUNT)
        break
    else:
        oldest = queue.pop()
