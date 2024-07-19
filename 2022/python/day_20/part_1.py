import sys
sys.path.append(".")
import base as b

def make_dict(l):
    return {idx:v for idx,v in enumerate(l)}


lines = b.get_input_first_lines("day_01")
data = [int(l) for l in lines]
initial_positions = make_dict(data)
curr_positions = {i:i for i in range(data)} # element at 0 is at 0, element at 1 is at 1, etc
for i in range(data):
    shift_value = initial_positions[i]
    idx = curr_positions[i]
    if shift_value > 0:
        if idx+shift_value >= len(data):
            new_idx = idx+shift_value-len(data)+1
            before = data[0:new_idx]
            after = data[new_idx:idx]
            list = [*before, shift_value, *after]
            # todo
        else:
            new_idx = idx+shift_value
    elif shift_value < 0:
        pass
