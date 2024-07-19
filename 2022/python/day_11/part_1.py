import sys
sys.path.append(".")
import base as b
from collections import deque as dq


class Monkey:
    def __init__(self, items, operation, test, when_true, when_false):
        self.items = self.parse_items(items)
        self.operation = self.parse_operation(operation)
        self.test = self.parse_test(test)
        self.when_true = self.parse_when_true(when_true)
        self.when_false = self.parse_when_false(when_false)
        self.items_inspected = 0

    def execute_turn(self,monkeys):
        self.items_inspected += len(self.items)
        while self.items:
            curr = self.items.popleft()
            curr = self.operation(curr)
            curr = curr // 3
            condition = self.test(curr)
            target_monkey_id = self.when_true if condition else self.when_false
            monkeys[target_monkey_id].items.append(curr)

    def parse_items(self,items):
        parts = items.split(":")
        return dq([int(i) for i in parts[1].split(",")])

    def parse_operation(self, operation):
        parts = operation.strip().split(":")
        parts = parts[1].split(" ")
        operator = parts[4]
        if operator == "+":
            if parts[5] == "old":
                return lambda x: x* 2
            else:
                return lambda x:x+int(parts[5])
        else:
            if parts[5] == "old":
                return lambda x: x*x
            else:
                return lambda x:x*int(parts[5])

    def parse_test(self, test):
        parts = test.strip().split(" ")
        return lambda x: x % int(parts[-1]) == 0
    
    def parse_when_true(self,when_true):
        parts = when_true.strip().split(" ")
        return int(parts[-1])

    def parse_when_false(self,when_false):
        parts = when_false.strip().split(" ")
        return int(parts[-1])

def partition(lst, size):
    for i in range(0, len(lst), size):
        yield lst[i : i+size]

def parse_monkeys(lines):
    monkeys = []
    for p in partition(lines, 7):
        monkeys.append(Monkey(*p[1:-1]))
    return monkeys

lines = b.get_input_first_lines("day_11")

monkeys = parse_monkeys(lines)
for i in range(20):
    for m in monkeys:
        m.execute_turn(monkeys)

sorted_monkeys = sorted(monkeys, key=lambda m:m.items_inspected)
top_1 = sorted_monkeys[-1]
top_2 = sorted_monkeys[-2]

print(top_1.items_inspected * top_2.items_inspected)
