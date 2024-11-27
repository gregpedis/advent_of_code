package utils

import (
	"bufio"
	"io/ioutil"
	"os"
)

func ReadLines(filePath string) []string {
	file, err := os.Open(filePath)
	if err != nil {
		panic(err)
	}
	defer file.Close()

	var lines = []string{}
	var scanner = bufio.NewScanner(file)

	for scanner.Scan() {
		var line = scanner.Text()
		lines = append(lines, line)
	}

	if err := scanner.Err(); err != nil {
		panic(err)
	}

	return lines
}

func ReadAll(filePath string) string {
	content, err := ioutil.ReadFile(filePath)

	if err != nil {
		panic(err)
	}

	return string(content)
}
