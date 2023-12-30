package day11

import (
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

const UP = 0
const RIGHT = 1
const DOWN = 2
const LEFT = 3

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)

	robotDirection := UP
	robotX := 0
	robotY := 0
	// y * 1000 + x or something for our keys, I guess strings would be less
	// prone to collisions but whatever.
	//
	// all panels are initially black = 0, white is 1.
	colors := make(map[string]int)
	input := make(chan int)
	output := make(chan int)
	var wg sync.WaitGroup

	wg.Add(2)
	go func() {
		intcode.ExecFull(program, input, output)
		defer wg.Done()
	}()
	go func() {
		input <- colors[fmt.Sprintf("%d,%d", robotX, robotY)]
	ProcessingLoop:
		for {
			newColor := <-output
			turnDirection := <-output
			colors[fmt.Sprintf("%d,%d", robotX, robotY)] = newColor
			if turnDirection == 0 {
				robotDirection -= 1
			} else {
				robotDirection += 1
			}
			robotDirection = (4 + robotDirection) % 4
			// move robot forward.
			switch robotDirection {
			case LEFT:
				robotX -= 1
			case RIGHT:
				robotX += 1
			case DOWN:
				robotY += 1
			case UP:
				robotY -= 1
			}

			// this is such a dumb hack.  how can I do this better?
			select {
			case input <- colors[fmt.Sprintf("%d,%d", robotX, robotY)]:
				// cool
			case <-time.After(100 * time.Millisecond):
				break ProcessingLoop
			}
		}
		defer wg.Done()
	}()
	wg.Wait()

	numPainted := 0
	for range colors {
		numPainted++
	}
	fmt.Println(numPainted)
}
