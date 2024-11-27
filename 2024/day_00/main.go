package main

import (
	"advent_of_code/utils"
	"fmt"
	"strconv"
	"unicode"
)

func main() {
	var lines = utils.ReadLines("day_00/input.txt")

	var res1 = solve(lines, solveLine1)
	fmt.Println("Part 1:", res1)

	var res2 = solve(lines, solveLine2)
	fmt.Println("Part 1:", res2)
}

func solve(lines []string, solveLine func(string) int) int {
	var res = 0
	for _, line := range lines {
		res += solveLine(line)
	}
	return res
}

func solveLine1(line string) int {
	var left rune
	var right rune
	for _, c := range line {
		if unicode.IsDigit(c) {
			if left == 0 {
				left = c
			}
			right = c
		}
	}

	var num, err = strconv.Atoi(string(left) + string(right))
	if err != nil {
		panic(err)
	}
	return num
}

func solveLine2(line string) int {
	var left rune
	var right rune
	for _, c := range line {
		if unicode.IsDigit(c) {
			if left == 0 {
				left = c
			}
			right = c
		}
	}

	var num, err = strconv.Atoi(string(left) + string(right))
	if err != nil {
		panic(err)
	}
	return num
}
