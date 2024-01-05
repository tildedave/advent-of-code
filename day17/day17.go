package day17

import (
	"fmt"
	"log"
	"math/rand"
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

func getTurnCommands(direction, nextDirection int) []robotCommand {
	if direction != nextDirection {
		if direction == reverseDirection(nextDirection) {
			// must do two right or left turns
			return []robotCommand{{turn: LEFT}, {turn: LEFT}}
		} else {
			return []robotCommand{{turn: getTurn(direction, nextDirection)}}
		}
	}
	return []robotCommand{}
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

func toGraphViz(nodeCoords map[int]coords, edges []edge) string {
	str := "digraph G {\n"
	for node, coords := range nodeCoords {
		str += fmt.Sprintf("\tNode%d [label=\"%d (%d, %d)\"];\n", node, node, coords.x, coords.y)
	}
	for _, e := range edges {
		str += fmt.Sprintf("\tNode%d -> Node%d [label=\"%d; %s -> %s\"];\n", e.source, e.dest, e.num, directionToString(e.direction), directionToString(e.endDirection))
	}
	str += "}\n"
	return str
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
	var endEdge edge
	for _, e := range edges {
		if e.dest == endNode || e.source == endNode {
			endEdge = e
		}
	}

	// so now we have a graph that we can get an eulerian tour on.
	// the tour algo is basically easy, start anywhere, continue until all
	// edges gone.  we need to avoid the start/end edges in this process.
	// I am not certain if I'll need to optimize this.

	fmt.Println(toGraphViz(nodeCoords, edges))

	numCircuitsFound := 0
	smallCircuits := make(map[string]int)
	smallCircuitSize := 500
	for numCircuitsFound < 1 {
		// random walk until we find the path.
		seenEdges := make(map[int]int)
		for _, e := range edges {
			seenEdges[e.num] = 0
		}
		edgesLeft := len(seenEdges)
		circuit := make([]robotCommand, 0)

		// follow a path from this node [how?] until we return to the node.
		// then join the path to our tour.

		currentNode := robotNode
		location := robotPosition
		var nextEdge edge
		direction := robotDirection

		visited := make([][]int, 0)
		for y := 0; y < len(grid); y++ {
			visited = append(visited, make([]int, len(grid[0])))
		}
		visited[robotPosition.y][robotPosition.x] = 1
	RandomWalk:
		for {
			if currentNode == endNode {
				if edgesLeft != 0 {
					break
				}
				// this is working correctly
				// fmt.Println("*******")
				// for y := 0; y < len(grid); y++ {
				// 	for x := 0; x < len(grid[0]); x++ {
				// 		if grid[y][x] == SCAFFOLD || grid[y][x] == ROBOT {
				// 			if visited[y][x] == 0 {
				// 				fmt.Println("Did not visit", x, y)
				// 				panic("Invalid circuit")
				// 			}
				// 		}
				// 		fmt.Print(visited[y][x])
				// 	}
				// 	fmt.Println()
				// }
				// fmt.Println("*******")
				numCircuitsFound++
				finalCircuit := make([]robotCommand, 0)
				currSteps := 0
				totalSteps := 0
				for _, c := range circuit {
					// combine steps
					if c.steps > 0 {
						currSteps += c.steps
						totalSteps += c.steps
					} else if c.turn != 0 {
						if currSteps > 0 {
							finalCircuit = append(finalCircuit, robotCommand{steps: currSteps})
							currSteps = 0
						}
						finalCircuit = append(finalCircuit, c)
					}
				}
				if currSteps > 0 {
					finalCircuit = append(finalCircuit, robotCommand{steps: currSteps})
					totalSteps += currSteps
				}

				str2 := commandsToString(circuit)
				if len(str2) < smallCircuitSize {
					smallCircuits[str2] = totalSteps
				}
				circuit = finalCircuit
				str := commandsToString(circuit)

				success1, _, _ := Compress(str2)
				if success1 {
					fmt.Println("OMFGFGFGFFGFG", str2)
				}
				success, _, _ := Compress(str)
				if success {
					fmt.Println("OMFGFGFGFFGFG", str)
				}

				fmt.Println("found circuit", str)
				if len(str) < smallCircuitSize {
					smallCircuits[str] = totalSteps
				}
				break
			}
			foundEdge := false
			if !foundEdge {
				numEdges := len(nodeEdges[currentNode])
				randomOffset := rand.Intn(numEdges)
				for n := range nodeEdges[currentNode] {
					idx := (n + randomOffset) % numEdges
					candidateEdge := nodeEdges[currentNode][idx]
					if seenEdges[candidateEdge.num] <= 1 && (candidateEdge.num != endEdge.num || edgesLeft == 1) {
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

			// add sanity checking here.
			prevDirection := direction
			if nextEdge.source == currentNode {
				// add path to circuit
				turns := getTurnCommands(direction, nextEdge.direction)
				for _, c := range turns {
					direction = makeTurn(direction, c.turn)
				}
				if direction != nextEdge.direction {
					panic("did not turn like we expected")
				}
				circuit = append(circuit, turns...)

				currentNode = nextEdge.dest
				for _, c := range nextEdge.path {
					if c.turn != 0 {
						direction = makeTurn(direction, c.turn)
					} else if c.steps > 0 {
						for n := 0; n < c.steps; n++ {
							location = moveInDirection(location, direction)
							if grid[location.y][location.x] == NOTHINGNESS {
								panic("We stepped off the scaffold - generating circuit")
							}
							visited[location.y][location.x] += 1
						}
					}
					circuit = append(circuit, c)
				}
			} else if nextEdge.dest == currentNode {
				// fmt.Printf("we are at (%d,%d) [node %d], going to (%d, %d) [node %d]\n", location.x, location.y, currentNode, nodeCoords[nextEdge.source].x, nodeCoords[nextEdge.source].y, nextEdge.source)
				// fmt.Println("the path is supposedly", commandsToString(reversePath(nextEdge.path)), " (originally ", commandsToString(nextEdge.path), ")")
				// fmt.Println("the path has directions start", directionToString(nextEdge.direction), "ending", directionToString(nextEdge.endDirection))

				// reverse the path when it's added to the circuit
				expectedDirection := reverseDirection(nextEdge.endDirection)
				turns := getTurnCommands(direction, expectedDirection)
				for _, c := range turns {
					direction = makeTurn(direction, c.turn)
				}
				if direction != expectedDirection {
					panic("did not turn like we expected")
				}
				circuit = append(circuit, getTurnCommands(prevDirection, direction)...)
				currentNode = nextEdge.source

				// sanity check the entire path
				for _, c := range reversePath(nextEdge.path) {
					if c.turn != 0 {
						direction = makeTurn(direction, c.turn)
					} else if c.steps > 0 {
						for n := 0; n < c.steps; n++ {
							location = moveInDirection(location, direction)
							if grid[location.y][location.x] == NOTHINGNESS {
								fmt.Println(commandsToString(reversePath(nextEdge.path)))
								panic("We stepped off the scaffold - reverse path")
							}
							visited[location.y][location.x] += 1
						}
					}
					circuit = append(circuit, c)
				}
			} else {
				panic("Invalid edge configuration")
			}

			if seenEdges[nextEdge.num] == 0 {
				edgesLeft--
			}

			seenEdges[nextEdge.num] += 1
		}
	}

	var commands string
	for s := range smallCircuits {
		commands = s
		fmt.Println(len(s), "found circuit", s)
	}

	input = make(chan int)
	output = make(chan int)
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		intcode.ExecFull(program, input, output)
		defer wg.Done()
	}()
	// let's try just feeding the commands to the robot and see if I've messed
	// something up horribly.
	program[0] = 2
	commands = "A,B,A,C,A,B,A,C,B,C"
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
	for _, chr := range commands {
		input <- int(chr)
	}
	input <- int('\n')
	line1 := "R,4,L,12,L,8,R,4"
	line2 := "L,8,R,10,R,10,R,6"
	line3 := "R,4,R,10,L,12"
	for _, line := range []string{line1, line2, line3} {
		fmt.Println(line)
		for o := range output {
			fmt.Print(string(rune(o)))
			if rune(o) == '\n' {
				fmt.Println("breaking")
				break
			}
		}
		for _, chr := range line {
			fmt.Println("sending", int(chr))
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
	// sadly the robot is doing what I expect it to so I need to implement
	// compression in order to find the compressable path.  ugh.
}
