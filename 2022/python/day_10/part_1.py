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


lines = b.get_input_first_lines("day_10")
instructions = get_instructions(lines)

register_values = apply_instructions(instructions)
target_cycles = [20,60,100,140,180,220]

res = sum(c * register_values[c] for c in target_cycles)
print(res)