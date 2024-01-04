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

type edge struct {
	direction    int
	endDirection int
	source       int
	dest         int
	path         []robotCommand
	num          int
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

func reversePath(path []robotCommand) []robotCommand {
	reversedPath := make([]robotCommand, len(path))
	for i, r := range path {
		if r.steps != 0 {
			reversedPath[len(path)-1-i] = r
		} else {
			if r.turn == LEFT {
				reversedPath[len(path)-1-i] = robotCommand{turn: RIGHT}
			} else {
				reversedPath[len(path)-1-i] = robotCommand{turn: LEFT}
			}
		}
	}
	return reversedPath
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

func toGraphViz(nodeCoords map[int]coords, edges []edge) string {
	str := "digraph G {\n"
	for node, coords := range nodeCoords {
		str += fmt.Sprintf("\tNode%d [label=\"%d (%d, %d)\"];\n", node, node, coords.x, coords.y)
	}
	for _, e := range edges {
		str += fmt.Sprintf("\tNode%d -> Node%d [label=\"%d\"];\n", e.source, e.dest, e.num)
	}
	str += "}\n"
	return str
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
	edgeNum := 0
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
				if grid[offsetY][offsetX] == SCAFFOLD || grid[offsetY][offsetX] == ROBOT {
					numAdjacent++
					dirs[n] = true
				}
			}
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

	edges := make([]edge, 0)
	for node, coords := range nodeCoords {
		d := validDirections[coords]
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
				location = moveInDirection(location, dir)
			}
			path = append(path, robotCommand{steps: numSteps})
			edgeNum++
			newEdge := edge{startDir, dir, node, nodeCoordsReverse[location], path, edgeNum}
			pathStr := commandsToString(path)
			hasReverseAlready := false
			for _, e := range edges {
				if e.dest == newEdge.source && e.source == newEdge.dest && commandsToString(reversePath(e.path)) == pathStr {
					hasReverseAlready = true
					break
				}
			}
			if !hasReverseAlready {
				edges = append(edges, newEdge)
			}
		}
	}

	// now we have everything.  we want an eulerian tour on this graph.
	// it should be easy to identify which is the end vertex.
	// I feel like in general any tour will be fine, the end result of this
	// is going to be a path that I need to manually convert into a program.
	// I'm sure there is some cooler way to do this automatically for general
	// inputs.

	robotNode := nodeCoordsReverse[robotPosition]
	// the ending node is the one that has only 1 degree and is not the robot
	// node.
	degrees := make(map[int]int)
	nodeEdges := make(map[int][]edge)
	for _, e := range edges {
		if nodeEdges[e.source] == nil {
			nodeEdges[e.source] = make([]edge, 0)
		}
		if nodeEdges[e.dest] == nil {
			nodeEdges[e.dest] = make([]edge, 0)
		}
		nodeEdges[e.source] = append(nodeEdges[e.source], e)
		nodeEdges[e.dest] = append(nodeEdges[e.dest], e)
		if e.source != e.dest {
			degrees[e.source]++
			degrees[e.dest]++
		}
	}
	endNode := -1
	for d, n := range degrees {
		if n == 1 && d != robotNode {
			endNode = d
		}
	}

	// so now we have a graph that we can get an eulerian tour on.
	// the tour algo is basically easy, start anywhere, continue until all
	// edges gone.  we need to avoid the start/end edges in this process.
	// I am not certain if I'll need to optimize this.

	numCircuitsFound := 0
	var minCircuit []robotCommand
	for numCircuitsFound < 10000 {
		// random walk until we find the path.
		seenEdges := make(map[int]bool)
		circuit := make([]robotCommand, 0)

		// follow a path from this node [how?] until we return to the node.
		// then join the path to our tour.

		currentNode := robotNode
		var nextEdge edge
		direction := robotDirection
	RandomWalk:
		for {
			if currentNode == endNode {
				numCircuitsFound++
				if len(minCircuit) == 0 || len(circuit) < len(minCircuit) {
					minCircuit = circuit
				}
				break
			}
			foundEdge := false
			// always do a self-loop if possible
			for _, candidateEdge := range nodeEdges[currentNode] {
				if !seenEdges[candidateEdge.num] && candidateEdge.source == candidateEdge.dest {
					// this is our edge
					foundEdge = true
					nextEdge = candidateEdge
					break
				}
			}
			if !foundEdge {
				for _, candidateEdge := range nodeEdges[currentNode] {
					if !seenEdges[candidateEdge.num] {
						nextEdge = candidateEdge
						foundEdge = true
						break
					}
				}
			}

			if !foundEdge {
				// restart walk
				break RandomWalk
			}

			if direction != nextEdge.direction {
				if direction == reverseDirection(nextEdge.direction) {
					// must do two right or left turns
					circuit = append(circuit, robotCommand{turn: LEFT})
					circuit = append(circuit, robotCommand{turn: LEFT})
				} else {
					circuit = append(circuit, robotCommand{turn: getTurn(direction, nextEdge.direction)})
				}
			}

			if nextEdge.source == currentNode {
				// add path to circuit
				currentNode = nextEdge.dest
				circuit = append(circuit, nextEdge.path...)
			} else if nextEdge.dest == currentNode {
				// reverse the path when it's added to the circuit
				currentNode = nextEdge.source
				circuit = append(circuit, reversePath(nextEdge.path)...)
			} else {
				panic("Invalid edge configuration")
			}

			direction = nextEdge.endDirection
			seenEdges[nextEdge.num] = true
		}
	}

	fmt.Println(directionToString(robotDirection))
	fmt.Println("found circuit", commandsToString(minCircuit), len(minCircuit))
	// we've found our circuit.  very exciting.
	// we need to validate that it works (probably)
	// and we need to compile it into a program.  each program can be 20 lines
	// at most.
	// I'm probably going to compile it manually?
	location := robotPosition
	direction := robotDirection
	fmt.Println(location)
	for _, command := range minCircuit {
		fmt.Println(command)
		if command.turn != 0 {
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
			if command.turn == RIGHT {
				direction = reverseDirection(direction)
			}
			fmt.Println("now facing", directionToString(direction), "at", location)
			continue
		}

		// otherwise we move forward n steps
		for n := 0; n < command.steps; n++ {
			location = moveInDirection(location, direction)
			if grid[location.y][location.x] == NOTHINGNESS {
				panic("We stepped off the scaffold")
			}
		}
	}

	fmt.Println(degrees, robotNode, endNode)

	fmt.Println(robotPosition.x, robotPosition.y, robotDirection)
	// is this it?
}
