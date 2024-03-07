package day17

import (
	"fmt"
	"log"
	"os"
	"strings"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

const SCAFFOLD = 1    // #
const NOTHINGNESS = 2 // '.'
const DEAD_ROBOT = 3  // 'X'
const ROBOT = 4       // one of ^ < > v

// robot orientation which I am sure I will use in part 2.
const UP = 0
const LEFT = 1
const RIGHT = 2
const DOWN = 3

type coords = struct {
	x int
	y int
}

type robotCommand = struct {
	steps int // 0 for turns
	turn  int // 1 = left, 2 = right
}

func moveInDirection(location coords, direction int) coords {
	switch direction {
	case UP:
		return coords{location.x, location.y - 1}
	case DOWN:
		return coords{location.x, location.y + 1}
	case LEFT:
		return coords{location.x - 1, location.y}
	case RIGHT:
		return coords{location.x + 1, location.y}
	default:
		panic("Invalid direction")
	}
}

func commandsToString(commands []robotCommand) string {
	str := ""
	for n, c := range commands {
		if n != 0 {
			str += ","
		}
		if c.turn != 0 {
			if c.turn == 1 {
				str += "L"
			} else if c.turn == 2 {
				str += "R"
			}
		} else {
			str += fmt.Sprintf("%d", c.steps)
		}
	}

	return str
}

func reverseDirection(direction int) int {
	switch direction {
	case UP:
		return DOWN
	case DOWN:
		return UP
	case LEFT:
		return RIGHT
	case RIGHT:
		return LEFT
	default:
		panic("Invalid direction")
	}
}

func makeTurn(direction int, turn int) int {
	// assume a left turn and just 180 in the case that it isn't.
	switch direction {
	case UP:
		direction = LEFT
	case DOWN:
		direction = RIGHT
	case LEFT:
		direction = DOWN
	case RIGHT:
		direction = UP
	}
	if turn == RIGHT {
		return reverseDirection(direction)
	}
	return direction
}

func contentToString(c int) string {
	switch c {
	case SCAFFOLD:
		return "#"
	case NOTHINGNESS:
		return "."
	case DEAD_ROBOT:
		return "X"
	case ROBOT:
		return "R"
	default:
		log.Fatalf("Did not recognize item: %d", c)
		return ""
	}
}

func Compress(str string) (bool, string, [3]string) {
	// find the first substring; it shouldn't end in a comma, but the next
	// character should be a comma.
	for aEnd := 0; aEnd <= 20; aEnd++ {
		if str[aEnd] != ',' {
			continue
		}
		candidateA := str[0:aEnd]
		// find the first non-A digit.
		// We might have A, A, whatever, so we have to look forward for a
		// while.
		// Let's assume we aren't as it makes my life a bit easier.
		// In the event that I can't find the path I will come back and fix this.

		bStart := aEnd + 1
		for bEnd := bStart; bEnd <= bStart+20 && bEnd < len(str); bEnd++ {
			if str[bEnd] != ',' {
				continue
			}
			candidateB := str[bStart:bEnd]

			// find the first non-A and non-B digit.  we have to hunt a bit
			// for this.
			for cStart := bEnd; cStart < len(str); cStart++ {
				// must start on an L, and R, or a number.
				r := int(str[cStart])
				if str[cStart] == 'L' || str[cStart] == 'R' || (r >= 48 && r <= 57) {
					// valid cStart.
					// find cEnd.
					for cEnd := cStart; cEnd <= cStart+20 && cEnd < len(str); cEnd++ {
						// it's possible we'll end on A - with my puzzle input I think I will.
						if str[cEnd] != ',' && cEnd != len(str)-1 {
							continue
						}
						candidateC := str[cStart:cEnd]
						workStr := str
						workStr = strings.ReplaceAll(workStr, candidateA, "A")
						workStr = strings.ReplaceAll(workStr, candidateB, "B")
						workStr = strings.ReplaceAll(workStr, candidateC, "C")
						isValid := true
						for _, chr := range workStr {
							if chr == 'L' || chr == 'R' || (chr >= 48 && chr <= 57) {
								isValid = false
							}
						}
						if isValid {
							fmt.Println("WorkStr compressed", workStr)
							fmt.Println(candidateA, candidateB, candidateC)
							return true, workStr, [3]string{candidateA, candidateB, candidateC}
						}
					}
				}
			}
		}
	}

	return false, "", [3]string{}
}

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	input := make(chan int)
	output := make(chan int)
	go func() {
		intcode.ExecFull(program, input, output)
		close(input)
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

	if !partTwo {
		// let's identify the intersections.
		scaffolds := make([][2]int, 0)
		for y := 0; y < len(grid); y++ {
			for x := 0; x < len(grid[0]); x++ {
				if grid[y][x] != SCAFFOLD {
					continue
				}
				isInvalid := false
				for _, d := range [][2]int{{0, -1}, {-1, 0}, {0, 1}, {1, 0}} {
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
		return
	}

	// nodeCoords := make(map[int]coords)
	// nodeCoordsReverse := make(map[coords]int)
	// validDirections := make(map[coords][4]bool)
	// nodeNum := 0
	// edgeNum := 0
	var robotPosition coords

	// // so for each node we store info between the edges along with the path it
	// // took to get there.
	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[0]); x++ {
			if grid[y][x] == ROBOT {
				robotPosition = coords{x, y}
			}
		}
	}

	// I used to have a much more complicated algorithm but apparently it's
	// fine to just walk forward until you have to turn.  I guess this makes
	// sense.
	canMove := func(location coords, direction, turn int) bool {
		if turn != 0 {
			direction = makeTurn(direction, turn)
		}
		forward := moveInDirection(location, direction)
		if forward.x < 0 || forward.x >= len(grid[0]) {
			return false
		}
		if forward.y < 0 || forward.y >= len(grid) {
			return false
		}
		return grid[forward.y][forward.x] == SCAFFOLD
	}

	location := robotPosition
	direction := robotDirection
	path := make([]robotCommand, 0)
	for {
		steps := 0
		for canMove(location, direction, 0) {
			location = moveInDirection(location, direction)
			steps += 1
		}
		if steps > 0 {
			path = append(path, robotCommand{steps: steps})
		}
		if canMove(location, direction, RIGHT) {
			path = append(path, robotCommand{turn: RIGHT})
			direction = makeTurn(direction, RIGHT)
		} else if canMove(location, direction, LEFT) {
			path = append(path, robotCommand{turn: LEFT})
			direction = makeTurn(direction, LEFT)
		} else {
			break
		}
	}

	result, mainProgram, subPrograms := Compress(commandsToString(path))
	fmt.Println(result)

	input = make(chan int)
	output = make(chan int)
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		intcode.ExecFull(program, input, output)
		defer wg.Done()
	}()

	program[0] = 2
	line := ""
	for o := range output {
		fmt.Print(string(rune(o)))
		line += string(rune(o))
		if o == int('\n') {
			line = ""
		}
		if line == "Main:" {
			<-output
			break
		}
	}
	for _, chr := range mainProgram {
		input <- int(chr)
	}
	input <- int('\n')
	for _, line := range subPrograms {
		fmt.Println(line)
		for o := range output {
			fmt.Print(string(rune(o)))
			if rune(o) == '\n' {
				break
			}
		}
		for _, chr := range line {
			input <- int(chr)
		}
		input <- int('\n')
	}
	for o := range output {
		fmt.Print(rune(o))
		if rune(o) == '\n' {
			break
		}
	}
	input <- int('n')
	input <- int('\n')
	for chr := range output {
		fmt.Print(chr)
	}
	wg.Wait()
	// prints out the number after a bunch of integers, 1010<actual number>
}
