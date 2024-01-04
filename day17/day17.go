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
const UP = 0
const LEFT = 1
const RIGHT = 2
const DOWN = 3

type coords = struct {
	x int
	y int
}

type directions = struct {
	up    bool
	down  bool
	left  bool
	right bool
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

func directionToString(direction int) string {
	switch direction {
	case LEFT:
		return "left"
	case RIGHT:
		return "right"
	case UP:
		return "up"
	case DOWN:
		return "down"
	default:
		panic("Invalid direction")
	}
}
func locationIsNotNode(location coords, nodeCoordsReverse map[coords]int) bool {
	_, ok := nodeCoordsReverse[location]
	return !ok
}

func getTurn(d1, d2 int) int {
	// return 0 - no turn
	if d1 == d2 {
		return 0
	}
	if d1 == reverseDirection(d2) {
		panic("Should be impossible")
	}
	if d1 == UP {
		// d2 must be left or right which match our return values
		return d2
	}
	if d1 == DOWN {
		// similar to d1 but in reverse.
		return reverseDirection(d2)
	}
	if d1 == LEFT {
		if d2 == UP {
			return RIGHT
		}
		if d2 == DOWN {
			return LEFT
		}
		panic("Impossible")
	}
	if d1 == RIGHT {
		if d2 == UP {
			return LEFT
		}
		if d2 == DOWN {
			return RIGHT
		}
		panic("Impossible")
	}
	// Well, this isn't the best code I've ever written.
	panic("Impossible")
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

	nodeCoords := make(map[int]coords)
	nodeCoordsReverse := make(map[coords]int)
	validDirections := make(map[coords][4]bool)
	nodeNum := 0
	var robotPosition coords

	// so for each node we store info between the edges along with the path it
	// took to get there.
	for y := 0; y < len(grid); y++ {
		for x := 0; x < len(grid[0]); x++ {
			if grid[y][x] == ROBOT {
				robotPosition = coords{x, y}
			} else if grid[y][x] != SCAFFOLD {
				continue
			}
			numAdjacent := 0
			location := coords{x, y}
			dirs := [4]bool{}
			for n, d := range [][2]int{{0, -1}, {-1, 0}, {1, 0}, {0, 1}} {
				offsetX, offsetY := x+d[0], y+d[1]
				if !(offsetX >= 0 && offsetX < len(grid[0]) && offsetY >= 0 && offsetY < len(grid)) {
					continue
				}
				if grid[offsetY][offsetX] == SCAFFOLD {
					numAdjacent++
					dirs[n] = true
				}
			}
			fmt.Println("directions from ", location, "are", dirs)
			validDirections[location] = dirs

			if numAdjacent != 2 {
				// it's a node
				nodeCoords[nodeNum] = location
				nodeCoordsReverse[location] = nodeNum
				fmt.Printf("!!! node %d discovered at (%d, %d) - %d neighbors\n", nodeNum, x, y, numAdjacent)
				nodeNum++
			}
		}
	}

	graphVizString := "digraph G {\n"
	for node, coords := range nodeCoords {
		d := validDirections[coords]
		graphVizString += fmt.Sprintf("\tNode%d [label=\"(%d, %d)\"];\n", node, coords.x, coords.y)
		for dir, able := range d {
			if !able {
				continue
			}

			// we also need to keep track of HOW this happened, which requires
			// understanding left or right turns (more helper functions)
			location := moveInDirection(coords, dir)
			numSteps := 1
			startDir := dir
			path := make([]robotCommand, 0)
			for locationIsNotNode(location, nodeCoordsReverse) {
				var nextDir int = -1
				for validDir, isOk := range validDirections[location] {
					if isOk && reverseDirection(validDir) != dir {
						nextDir = validDir
						break
					}
				}
				if nextDir == -1 {
					panic("Should be impossible")
				}

				turn := getTurn(dir, nextDir)
				if turn == 0 {
					numSteps++
				} else {
					path = append(path, robotCommand{steps: numSteps})
					// now we turn
					path = append(path, robotCommand{turn: turn})
					numSteps = 1
				}
				dir = nextDir
				fmt.Println("moving direction", dir, "from", location)
				location = moveInDirection(location, dir)
				fmt.Println("new location", location)
			}
			path = append(path, robotCommand{steps: numSteps})
			fmt.Printf("I connected node %d (%d, %d) with node %d (%d, %d)\n",
				node, coords.x, coords.y, nodeCoordsReverse[location], location.x, location.y)
			graphVizString += fmt.Sprintf("\tNode%d -> Node%d [label=\"starting %s and ending %s; %s\"];\n", node, nodeCoordsReverse[location], directionToString(startDir), directionToString(dir), commandsToString(path))
		}

	}
	graphVizString += "}\n"
	// so now we walk along each node until we find another node.

	fmt.Println(robotPosition.x, robotPosition.y, robotDirection)
	fmt.Println(graphVizString)
	// is this it?
}
