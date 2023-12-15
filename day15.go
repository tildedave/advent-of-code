package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func holidayASCIIStringHelper(s string) int {
	val := 0
	for i, _ := range s {
		val += int(s[i])
		val = val * 17
		val = val % 256
	}

	return val
}

func day15(f *os.File) {
	scanner := bufio.NewScanner(f)
	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		fields := strings.Split(line, ",")
		for _, r := range fields {
			sum += holidayASCIIStringHelper(r)
		}
	}
	fmt.Println(sum)
}
