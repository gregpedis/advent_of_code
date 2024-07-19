import sys
sys.path.append(".")
import base as b
from functools import cmp_to_key
from collections import deque as dq


DIGITS = "0123456789"

# [[],[1],[[[1,3],2,1,3]]]

# INT = 0-9
# LIST = [] | [elements] 
# elements = element | element, elements
# element = INT | LIST

def parse_int(q:dq, curr:str):
    res = curr
    while q:
        curr = q.popleft()
        if curr in DIGITS:
            res+=curr
        else:
            q.appendleft(curr)
            return int(res)
    return int(res)

def parse_list(q:dq):
    res = []
    while q:
        curr = q.popleft()
        if curr == "]":
            return res
        elif curr == "[":
            res.append(parse_list(q))
        elif curr in DIGITS:
            res.append(parse_int(q, curr))

def parse(line):
    qq = dq(line)
    while qq:
        curr = qq.popleft()
        if curr == "[":
            return parse_list(qq)
        else:
            parse_int(qq, curr)


def compare(left, right):
    if type(left) == type(right):
        if type(left) == int:
            return left-right
        else:
            for l,r in zip(left,right):
                res = compare(l,r)
                if res != 0:
                    return res
            return len(left) - len(right)
    elif type(left) == int:
        return compare([left], right)
    else:
        return compare(left, [right])

def get_pairs(lines):
    for i in range(0, len(lines),3):
        left,right = lines[i], lines[i+1]
        yield (parse(left), parse(right))


KEY_1 = "[[2]]"
KEY_2 = "[[6]]"
lines = b.get_input_first_lines("day_13")
lines.append("\n")
lines.append(KEY_1)
lines.append(KEY_2)
lines.append("\n")

flattened = [part for pair in get_pairs(lines) for part in pair]
sorted_flat = sorted(flattened, key=cmp_to_key(compare))
strings = [str(x) for x in sorted_flat]

idx_1 = strings.index(KEY_1)+1
idx_2 = strings.index(KEY_2)+1

print(idx_1 * idx_2)