package day9

import (
	"fmt"
	"os"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	input := make(chan int)
	output := make(chan int)
	if !partTwo {
		go intcode.ExecFull(program, input, output)
		// test mode
		input <- 1
		for o := range output {
			fmt.Println(o)
		}
	} else {
		go intcode.ExecFull(program, input, output)
		input <- 2
		for o := range output {
			fmt.Println(o)
		}

	}
}
