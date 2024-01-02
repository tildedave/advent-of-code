package utils

import (
	"bufio"
	"log"
	"os"
	"strconv"
	"strings"
)

func ParseNumberList(strlist []string) ([]int, error) {
	ret := make([]int, 0)
	for _, nStr := range strlist {
		n, err := strconv.ParseInt(nStr, 10, 64)
		if err != nil {
			return []int{}, err
		}
		ret = append(ret, int(n))
	}

	return ret, nil
}

func ParseProgram(f *os.File) []int {
	scanner := bufio.NewScanner(f)
	var program []int
	rows := 0
	for scanner.Scan() {
		if rows > 0 {
			panic("Only should have had one line of input")
		}
		line := scanner.Text()
		p, err := ParseNumberList(strings.Split(line, ","))
		if err != nil {
			log.Fatal(err)
		}
		program = p
		rows++
	}
	return program
}

func ReadSingleLine(f *os.File) string {
	var line string
	rows := 0

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if rows > 0 {
			panic("Can only process one row")
		}
		line = scanner.Text()
		rows++
	}
	return line
}

// https://stackoverflow.com/a/30230552
func NextPermutation(p []int) {
	for i := len(p) - 1; i >= 0; i-- {
		if i == 0 || p[i] < len(p)-i-1 {
			p[i]++
			return
		}
		p[i] = 0
	}
}

func GetPermutation(original, p []int) []int {
	result := make([]int, len(original))
	copy(result, original)

	for i, v := range p {
		result[i], result[i+v] = result[i+v], result[i]
	}
	return result
}

func AbsInt(i int) int {
	if i < 0 {
		return -i
	}
	return i
}
