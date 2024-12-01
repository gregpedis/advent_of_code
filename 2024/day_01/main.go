package main

import (
	"advent_of_code/utils"
	"fmt"
	"sort"
	"strconv"
	"strings"
)

func main() {
	var lines = utils.ReadLines("day_01/input.txt")

	var res1 = solve1(lines, extract_numbers)
	fmt.Println("Part 1:", res1)

	var res2 = solve2(lines, extract_numbers)
	fmt.Println("Part 2:", res2)
}

func solve1(lines []string, solveLine func(string) (int, int)) int {
	var left_values = []int{}
	var right_values = []int{}

	for _, line := range lines {
		var left, right = extract_numbers(line)
		left_values = append(left_values, left)
		right_values = append(right_values, right)
	}

	sort.Ints(left_values)
	sort.Ints(right_values)

	var total = 0
	for i := 0; i < len(left_values); i++ {
		total += absInt(left_values[i] - right_values[i])
	}
	return total
}

func solve2(lines []string, solveLine func(string) (int, int)) int {
	var left_values = []int{}
	var right_values = make(map[int]int)

	for _, line := range lines {
		var left, right = extract_numbers(line)
		left_values = append(left_values, left)
		right_values[right]++
	}

	var total = 0
	for i := 0; i < len(left_values); i++ {
		var left = left_values[i]
		total += left * right_values[left]
	}
	return total
}

func extract_numbers(line string) (int, int) {
	var splitted = strings.Fields(line)
	var left, err1 = strconv.Atoi(splitted[0])
	var right, err2 = strconv.Atoi(splitted[1])

	if err1 != nil {
		panic(err1)
	}
	if err2 != nil {
		panic(err2)
	}
	return left, right
}

func absInt(value int) int {
	if value < 0 {
		return -value
	} else {
		return value
	}
}
