package day7

import (
	"os"

	"github.com/tildedave/advent-of-code-2019/intcode"
)

func Run(f *os.File, partTwo bool) {
	h := make(chan bool)
	input := make(chan int, 1)
	output := make(chan int, 1)
	program := []int{3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0}
	go intcode.ExecFull(program, input, output, h)
}
