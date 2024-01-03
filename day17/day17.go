package day17

import (
	"fmt"
	"log"
	"os"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

const SCAFFOLD = 1    // #
const NOTHINGNESS = 2 // '.'
const DEAD_ROBOT = 3  // 'X'
const ROBOT = 4       // one of ^ < > v

// robot orientation which I am sure I will use in part 2.
const UP = 1
const LEFT = 2
const RIGHT = 3
const DOWN = 4

func contentToString(c int) string {
	switch c {
	case SCAFFOLD:
		return "#"
	case NOTHINGNESS:
		return "."
	case DEAD_ROBOT:
		return "X"
	case ROBOT:
		return "🤖"
	default:
		log.Fatalf("Did not recognize item: %d", c)
		return ""
	}
}

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	input := make(chan int)
	output := make(chan int)
	go func() {
		intcode.ExecFull(program, input, output)
	}()

	x := 0
	y := 0
	grid := make([][]int, 0)
	row := make([]int, 0)
	var robotDirection int

	// we rely on the kindness of the programmer that the grid is n x n.
	for a := range output {
		switch a {
		case '.':
			row = append(row, NOTHINGNESS)
			x++
		case '#':
			row = append(row, SCAFFOLD)
			x++
		case '^':
			row = append(row, ROBOT)
			robotDirection = UP
			x++
		case 'v':
			row = append(row, ROBOT)
			robotDirection = DOWN
			x++
		case '<':
			row = append(row, ROBOT)
			robotDirection = LEFT
			x++
		case '>':
			row = append(row, ROBOT)
			robotDirection = RIGHT
			x++
		case '\n':
			if len(row) > 0 {
				grid = append(grid, row)
			}
			row = make([]int, 0)
			y++
		}
	}

	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[0]); x++ {
			fmt.Print(contentToString(grid[y][x]))
		}
		fmt.Println()
	}

	// let's identify the intersections.
	scaffolds := make([][2]int, 0)
	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[0]); x++ {
			if grid[y][x] != SCAFFOLD {
				continue
			}
			isInvalid := false
			for _, d := range [][2]int{{0, -1}, {-1, 0}, {0, 1}, {0, 1}} {
				offsetX, offsetY := x+d[0], y+d[1]
				if !(offsetX >= 0 && offsetX < len(grid[0]) && offsetY >= 0 && offsetY < len(grid)) {
					isInvalid = true
					continue
				}
				if grid[offsetY][offsetX] != SCAFFOLD {
					isInvalid = true
					continue
				}
			}
			if !isInvalid {
				scaffolds = append(scaffolds, [2]int{x, y})
			}
		}
	}
	sum := 0
	for _, s := range scaffolds {
		sum += s[0] * s[1]
	}
	fmt.Println(sum)

	// is this it?
	fmt.Println(robotDirection)
}
