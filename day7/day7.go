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
			outputs := make([]chan int, 0)
			inputs := make([]chan int, 0)
			quits := make([]chan bool, 0)
			var result int

			for _, i := range perm {
				input := make(chan int, 1)
				output := make(chan int, 1)
				quit := make(chan bool)
				outputs = append(outputs, output)
				inputs = append(inputs, input)
				quits = append(quits, quit)
				wg.Add(1)
				go func(i int, input, output chan int) {
					intcode.ExecFull(program, input, output)
					defer wg.Done()
				}(i, input, output)
				input <- i
			}
			for n := range perm {
				go func(n int) {
					for {
						select {
						case o := <-outputs[n]:
							if n == len(outputs)-1 {
								result = o
							} else {
								inputs[n+1] <- o
							}
						case <-quits[n]:
							return
						}
					}
				}(n)
			}
			inputs[0] <- 0
			wg.Wait()
			for _, quit := range quits {
				quit <- true
				close(quit)
			}
			if result > maxResult {
				maxResult = result
			}
			for _, input := range inputs {
				close(input)
			}
			for _, output := range outputs {
				close(output)
			}
		}
		fmt.Println(maxResult)
		return
	}

	permutation := []int{5, 6, 7, 8, 9}
	for p := make([]int, len(permutation)); p[0] < len(p); utils.NextPermutation(p) {
		perm := utils.GetPermutation(permutation, p)

		var wg sync.WaitGroup
		outputs := make([]chan int, 0)
		inputs := make([]chan int, 0)
		quits := make([]chan bool, 0)
		finalOutputs := make([]int, 0)

		for _, i := range perm {
			input := make(chan int, 1)
			output := make(chan int, 1)
			quit := make(chan bool)
			outputs = append(outputs, output)
			inputs = append(inputs, input)
			quits = append(quits, quit)
			wg.Add(1)
			go func(input, output chan int) {
				intcode.ExecFull(program, input, output)
				defer wg.Done()
			}(input, output)
			input <- i
		}
		for n := range perm {
			go func(n int) {
				for {
					select {
					case o := <-outputs[n]:
						if n == len(outputs)-1 {
							finalOutputs = append(finalOutputs, o)
						}
						inputs[(n+1)%len(inputs)] <- o
					case <-quits[n]:
						return
					}
				}
			}(n)
		}
		inputs[0] <- 0
		wg.Wait()
		// consume output from the final thing??
		for _, quit := range quits {
			quit <- true
			close(quit)
		}
		for _, input := range inputs {
			close(input)
		}
		for _, output := range outputs {
			close(output)
		}
		if finalOutputs[len(finalOutputs)-1] > maxResult {
			maxResult = finalOutputs[len(finalOutputs)-1]
		}
	}
	fmt.Println(maxResult)
}
