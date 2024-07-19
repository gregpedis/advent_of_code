import sys
sys.path.append(".")
import base as b
from collections import deque as dq


class File:
    def __init__(self, name, size):
        self.name = name
        self.size = int(size)


class Directory:
    def __init__(self, parent, name):
        self.parent = parent
        self.name = name
        self.files = []
        self.directories = []

    def add_file(self, file):
        self.files.append(file)

    def add_directory(self, directory):
        self.directories.append(directory)

    def get_size(self):
        total = 0
        for f in self.files:
            total += f.size
        for d in self.directories:
            total += d.get_size()
        return total

    def __hash__(self) -> int:
        return hash(self.parent + "/" + self.name)

    def __eq__(self, other) -> bool:
        if type(other) is Directory:
            return self.name == other.name and self.parent == other.parent
        return False


def parse_cd(current_path: str, new_suffix: str):
    if new_suffix == "..":
        new_path = "/".join(current_path.split("/")[:-1])
    else:
        new_path = current_path+"/" + new_suffix
    return new_path


def parse_ls(current_path: str, lines: dq, directories: dict):
    while lines and not lines[0].startswith("$"):
        curr = lines.popleft()
        if curr.startswith("dir"):
            d = Directory(current_path, curr.replace("dir ", ""))
            directories[d.__hash__()] = d
            directories[hash(current_path)].add_directory(d)
        else:
            parts = curr.split(" ")
            file_size, file_name = parts[0], parts[1]
            f = File(file_name, file_size)
            directories[hash(current_path)].add_file(f)


lines = dq(b.get_input_first_lines("day_07"))
lines.popleft()
current_path = "/"
directories = {hash(current_path):Directory("", current_path)}

while lines:
    if lines[0].startswith("$ cd"):
        curr = lines.popleft()
        current_path = parse_cd(current_path, curr.replace("$ cd ", ""))
        pass
    elif lines[0].startswith("$ ls"):
        lines.popleft()
        parse_ls(current_path, lines, directories)

total = sum([d.get_size() for d in directories.values() if d.get_size()<=100000])
print(total)