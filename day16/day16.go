package day16

import (
	"fmt"
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/utils"
)

func onesDigitOnly(n int) int {
	if n < 0 {
		return (-n) % 10
	}
	return n % 10
}

func runPattern(n int, pattern []int, inputList []int) int {
	// we won't actually expand the pattern. each element in the pattern
	// gets repeated, we'll have a sub-variable to tell where we are in the
	// pattern.

	// this will be annoying for the n = 0 case but whatever.
	// j is the position in the pattern (pattern[j] is the actual value)
	// p is the sub-position in the pattern.  (increment p until p % n == 0)
	var j int
	var p int
	if n == 0 {
		j = 1
		p = 0
	} else {
		j = 0
		p = 1 // always skip the first element
	}

	sum := 0

	for _, c := range inputList {
		sum += pattern[j] * c

		p = (p + 1) % (n + 1)
		if p == 0 {
			j = (j + 1) % len(pattern)
		}
	}

	return onesDigitOnly(sum)
}

func RunPhase(input []int, pattern []int) []int {
	result := make([]int, len(input))
	for n := 0; n < len(input); n++ {
		result[n] = runPattern(n, pattern, input)
	}
	return result
}

func RunPhaseMultiple(input []int, pattern []int, times int) []int {
	for i := 0; i < times; i++ {
		input = RunPhase(input, pattern)
	}
	return input
}

func Run(f *os.File, partTwo bool) {
	line := utils.ReadSingleLine(f)
	inputList := make([]int, len(line))
	for i, c := range line {
		b := byte(c)
		if !(b >= 48 && b <= 57) {
			log.Fatalf("Invalid character in input list: %x", b)
		}
		inputList[i] = int(b - 48)
	}

	result := RunPhaseMultiple(inputList, []int{0, 1, 0, -1}, 100)
	for i := 0; i < 8; i++ {
		fmt.Printf("%d", result[i])
	}
	fmt.Println()
}
