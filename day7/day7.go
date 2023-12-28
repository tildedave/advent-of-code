package day7

import (
	"fmt"
	"os"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)

	maxResult := -1

	if !partTwo {
		permutation := []int{0, 1, 2, 3, 4}
		for p := make([]int, len(permutation)); p[0] < len(p); utils.NextPermutation(p) {
			perm := utils.GetPermutation(permutation, p)
			var wg sync.WaitGroup
			outputs := make([]chan int, len(perm))
			systemInput := make(chan int)
			for n := range perm {
				outputs[n] = make(chan int)
			}
			var result int

			for n, i := range perm {
				var input chan int
				if n == 0 {
					input = systemInput
				} else {
					input = outputs[n-1]
				}
				wg.Add(1)
				go func(i int, input, output chan int) {
					intcode.ExecFull(program, input, output)
					defer wg.Done()
				}(i, input, outputs[n])
				input <- i
			}
			systemInput <- 0
			for result = range outputs[len(perm)-1] {
			}
			wg.Wait()
			if result > maxResult {
				maxResult = result
			}
			close(systemInput)
		}
		fmt.Println(maxResult)
		return
	}

	permutation := []int{5, 6, 7, 8, 9}
	for p := make([]int, len(permutation)); p[0] < len(p); utils.NextPermutation(p) {
		// so here we the extra listener
		perm := utils.GetPermutation(permutation, p)
		var wg sync.WaitGroup
		outputs := make([]chan int, len(perm))
		systemInput := make(chan int)
		for n := range perm {
			outputs[n] = make(chan int)
		}
		var done bool

		for n, i := range perm {
			var input chan int
			if n == 0 {
				input = systemInput
			} else {
				input = outputs[n-1]
			}
			wg.Add(1)
			go func(n int, input, output chan int) {
				intcode.ExecFull(program, input, output)
				if n == 0 {
					// Needing to resort to this probably is a bug in how I'm
					// thinking about go channels.
					close(input)
					done = true
				}
				defer wg.Done()
			}(n, input, outputs[n])
			input <- i
		}
		var finalOutput int

		// This goroutine copies output from the final box back to the input.
		wg.Add(1)
		go func() {
			for o := range outputs[len(perm)-1] {
				finalOutput = o
				if !done {
					systemInput <- o
				}
			}
			defer wg.Done()
		}()
		systemInput <- 0
		wg.Wait()
		if finalOutput > maxResult {
			maxResult = finalOutput
		}
	}
	fmt.Println(maxResult)
}
