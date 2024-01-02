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

	if !partTwo {
		result := RunPhaseMultiple(inputList, []int{0, 1, 0, -1}, 1)
		for i := 0; i < 8; i++ {
			fmt.Printf("%d", result[i])
		}
		fmt.Println()
		return
	}

	// part two seems to require too much calculation to run the phases directly.
	// however, the pattern is fairly degenerate by the 7-digit message offset.
	// we can probably calculate just the output bits we care about.

	numRepetitions := 10_000
	sevenDigitOffset := 0
	for _, c := range inputList[0:7] {
		sevenDigitOffset *= 10
		sevenDigitOffset += c
	}
	var totalLength int = len(inputList) * numRepetitions

	if sevenDigitOffset*2 < totalLength {
		panic("Shortcut won't work")
	}

	digit := make([]int, 101)
	final := make([]int, 8)
	for i := 0; i <= 100; i++ {
		// needs to be the final element of the final output list, which is
		// just the last element of the input list
		digit[i] = inputList[len(inputList)-1]
	}

	/*
			[0 0 5 2 4 7 1 8 2 8 2 8 8 3 6 2 3 7 4 4 8 3 7 0 2 1 9 6 2 7 1 4 6 5 3 0 6 1 5 8]
		    [2 3 8 7 5 1 0 5 1 3 1 3 4 9 4 6 4 9 8 0 4 6 3 6 6 4 3 4 8 6 9 8 4 8 3 0 0 4 3 8]
		    [4 5 7 4 4 9 3 8 8 6 2 5 8 2 5 6 2 9 0 9 7 3 7 4 8 2 8 5 1 3 7 8 0 6 8 5 5 5 1 8]
		    [1 8 4 6 0 3 3 4 2 3 7 9 1 2 6 1 3 5 6 2 1 4 1 4 0 2 0 2 7 6 3 6 8 8 2 4 9 4 9 8]
		    [9 4 0 0 6 6 3 8 8 6 9 7 9 7 4 7 0 7 5 2 8 7 3 2 8 8 6 6 4 7 1 8 2 4 6 4 0 1 7 8]
		    [0 9 5 2 2 6 4 4 3 1 3 6 4 4 7 1 4 4 1 4 0 2 5 2 0 2 4 8 2 8 1 0 2 0 6 0 6 6 5 8]
		    [8 3 6 2 5 2 1 3 2 1 5 7 5 0 6 8 9 1 3 3 7 7 5 0 8 8 6 2 4 2 4 3 3 1 1 5 5 9 3 8]
		    [6 4 7 4 9 2 4 3 7 9 9 1 1 1 7 7 5 8 7 6 1 4 7 2 2 4 6 0 8 4 2 8 5 2 1 0 5 0 1 8]

			Each item in the back half of the table = item directly above + item directly to right % 10.
			We go down each row (there are 101 items as n = 0 is the original input list).
	*/
	for n := 1; n <= totalLength-sevenDigitOffset-1; n++ {
		for i := 0; i <= 100; i++ {
			if i == 0 {
				// this needs to be somehow smartly read from the inputList.
				// let's make it kind of dumb like this.
				idx := len(inputList) - 1 - n
				for idx < 0 {
					idx += len(inputList)
				}
				digit[i] = inputList[idx]
			} else {
				digit[i] = (digit[i] + digit[i-1]) % 10
			}
		}
		diffFromEnd := totalLength - sevenDigitOffset - 1 - n
		if diffFromEnd < 8 {
			final[diffFromEnd] = digit[100]
		}
	}
	fmt.Println(final)
}
