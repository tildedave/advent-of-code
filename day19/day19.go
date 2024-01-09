package day19

import (
	"fmt"
	"os"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)

	var wg sync.WaitGroup

	numOnes := 0
	for y := 0; y < 50; y++ {
		for x := 0; x < 50; x++ {
			input := make(chan int)
			output := make(chan int)

			wg.Add(1)
			go func() {
				intcode.ExecFull(program, input, output)
				defer wg.Done()
			}()
			input <- x
			input <- y
			c := <-output
			fmt.Print(c)
			if c == 1 {
				numOnes++
			}
			wg.Wait()
		}
		fmt.Println()
	}
	fmt.Println(numOnes)
}
