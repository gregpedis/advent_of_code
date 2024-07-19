import os
import os.path as pp

root = os.getcwd()
INPUT_FIRST_FN = "input_1"
INPUT_SECOND_FN = "input_2"


def get_input_first_raw(day):
    fn = _get_fn_path(root, day, INPUT_FIRST_FN)
    return _get_input_raw(fn)


def get_input_first_lines(day):
    fn = _get_fn_path(root, day, INPUT_FIRST_FN)
    return _get_input_lines(fn)


def get_input_second_raw(day):
    fn = _get_fn_path(root, day, INPUT_SECOND_FN)
    return _get_input_raw(fn)


def get_input_second_lines(day):
    fn = _get_fn_path(root, day, INPUT_SECOND_FN)
    return _get_input_lines(fn)


def _get_fn_path(root, day, fn):
    return pp.join(root, day, fn)


def _get_input_raw(fn):
    with open(fn, 'rt') as f:
        return f.read()


def _get_input_lines(fn):
    with open(fn, 'rt') as f:
        return [line.replace("\n","") for line in f.readlines()]
