package day23

import (
	"fmt"
	"os"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	numComputers := 50
	bufferSize := 100
	// we want to fan in outputs to a single channel where we can send
	// to the appropriate channels.
	fanInOutput := make(chan [4]int)

	inputChannels := make([]chan int, numComputers)
	outputChannels := make([]chan int, numComputers)
	for n := 0; n < numComputers; n++ {
		output := make(chan int, bufferSize)
		input := make(chan int, bufferSize)
		inputChannels[n] = input
		outputChannels[n] = output

		go func(n int) {
			for dest := range output {
				x := <-output
				y := <-output
				fanInOutput <- [4]int{n, dest, x, y}
			}
		}(n)

		go func(n int) {
			intcode.ExecFull(program, input, output, -1)
			// do we need to wait for completion?
		}(n)

		input <- n
	}

	for packet := range fanInOutput {
		_, dest, x, y := packet[0], packet[1], packet[2], packet[3]
		if dest == 255 {
			// done
			fmt.Println(y)
			break
		}
		inputChannels[dest] <- x
		inputChannels[dest] <- y
	}
}
