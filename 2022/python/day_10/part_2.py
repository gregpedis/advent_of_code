import sys
sys.path.append(".")
import base as b

class Noop:
    def execute(self, register_values:list):
        register_values.append(register_values[-1])

class AddX:
    def __init__(self, amount) -> None:
        self.amount = amount

    def execute(self, register_values:list):
        register_values.append(register_values[-1])
        register_values.append(register_values[-1] + self.amount)



def get_instructions(lines):
    instructions = []
    for l in lines:
        if l == "noop":
            instructions.append(Noop())
        else:
            amount = int(l.split(" ")[1])
            instructions.append(AddX(amount))
    return instructions

def apply_instructions(instructions):
    register_values = [1,1]
    for i in instructions:
        i.execute(register_values)
    return register_values

def move_crt(register_values, output):
    for idx, v in enumerate(register_values[1:-1]):
        pixels = [v-1,v,v+1]
        row = idx // 40
        col = idx % 40
        if col in pixels:
            output[row][col] = "#"

lines = b.get_input_first_lines("day_10")
instructions = get_instructions(lines)

register_values = apply_instructions(instructions)
output = [["."] *40 for i in range(6)]
move_crt(register_values, output)

print("".join(["".join(row) +"\n" for row in output]))