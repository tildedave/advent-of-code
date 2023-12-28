package day7

import (
	"fmt"
	"os"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	permutation := []int{0, 1, 2, 3, 4}

	maxResult := -1

	for p := make([]int, len(permutation)); p[0] < len(p); utils.NextPermutation(p) {
		perm := utils.GetPermutation(permutation, p)
		input := make(chan int, 1)
		output := make(chan int, 1)

		output <- 0
		for _, i := range perm {
			go intcode.ExecFull(program, input, output)
			input <- i
			input <- <-output
		}
		result := <-output
		if result > maxResult {
			maxResult = result
		}
		close(input)
		close(output)
	}
	fmt.Println(maxResult)
}
