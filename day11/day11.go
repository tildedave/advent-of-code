package day11

import (
	"fmt"
	"math"
	"os"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

const UP = 0
const RIGHT = 1
const DOWN = 2
const LEFT = 3

func key(x int, y int) string {
	return fmt.Sprintf("%d,%d", x, y)
}

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)

	robotDirection := UP
	robotX := 0
	robotY := 0
	minX := math.MaxInt
	maxX := math.MinInt
	minY := math.MaxInt
	maxY := math.MinInt

	// y * 1000 + x or something for our keys, I guess strings would be less
	// prone to collisions but whatever.
	//
	// all panels are initially black = 0, white is 1.
	colors := make(map[string]int)
	input := make(chan int)
	output := make(chan int)
	quit := make(chan bool)
	var wg sync.WaitGroup

	if partTwo {
		colors[key(robotX, robotY)] = 1
	}

	wg.Add(2)
	go func() {
		intcode.ExecFull(program, input, output)
		quit <- true
		defer wg.Done()
	}()
	go func() {
		input <- colors[key(robotX, robotY)]
	ProcessingLoop:
		for {
			newColor := <-output
			turnDirection := <-output
			colors[key(robotX, robotY)] = newColor
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
			if robotX > maxX {
				maxX = robotX
			}
			if robotX < minX {
				minX = robotX
			}
			if robotY > maxY {
				maxY = robotY
			}
			if robotY < minY {
				minY = robotY
			}

			select {
			case <-quit:
				break ProcessingLoop
			case input <- colors[key(robotX, robotY)]:
				// cool
			}
		}
		defer wg.Done()
	}()
	wg.Wait()

	numPainted := 0
	for range colors {
		numPainted++
	}

	str := ""
	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			color := colors[key(x, y)]
			if color == 0 {
				str += "."
			} else if color == 1 {
				str += "#"
			} else {
				panic("Invalid color")
			}
		}
		str += "\n"
	}
	fmt.Println(str)
	fmt.Println(numPainted)
}
