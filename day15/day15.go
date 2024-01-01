package day15

import (
	"container/heap"
	"fmt"
	"log"
	"math"
	"os"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

const UNKNOWN = 0
const HALLWAY = 1
const WALL = 2
const OXYGEN_SYSTEM = 4

const NORTH = 1
const SOUTH = 2
const WEST = 3
const EAST = 4

func reverseDirection(direction int) int {
	switch direction {
	case NORTH:
		return SOUTH
	case SOUTH:
		return NORTH
	case WEST:
		return EAST
	case EAST:
		return WEST
	default:
		panic("Invalid direction")
	}
}

func getScore(score map[int]int, node int) int {
	val, ok := score[node]
	if !ok {
		return math.MaxInt
	}
	return val
}

func getDirection(n1, n2 node) int {
	diff := node{n2.x - n1.x, n2.y - n1.y}
	if diff.x == -1 && diff.y == 0 {
		return WEST
	}
	if diff.x == 1 && diff.y == 0 {
		return EAST
	}
	if diff.x == 0 && diff.y == -1 {
		return NORTH
	}
	if diff.x == 0 && diff.y == 1 {
		return SOUTH
	}

	panic("Asked for direction between two non-adjacent nodes")
}

func PrintKnowledgeOfArea(nodeContents map[node]int, reverseNodeCoords map[node]int) {
	minX := math.MaxInt
	minY := math.MaxInt
	maxX := math.MinInt
	maxY := math.MinInt
	for v := range nodeContents {
		if v.x > maxX {
			maxX = v.x
		}
		if v.x < minX {
			minX = v.x
		}
		if v.y > maxY {
			maxY = v.y
		}
		if v.y < minY {
			minY = v.y
		}
	}
	str := ""
	for y := minY; y <= maxY; y++ {
		for x := minX; x <= maxX; x++ {
			switch nodeContents[node{x, y}] {
			case UNKNOWN:
				str += "?"
			case WALL:
				str += "#"
			case OXYGEN_SYSTEM:
				str += "D"
			case HALLWAY:
				str += "."
			}
		}
		str += "\n"
	}
	fmt.Println(str)
}

type NodeHeap struct {
	score    map[int]int
	contents map[int]bool
	slice    []int
}

type Neighbors = struct {
	north int
	south int
	east  int
	west  int
}

type node = struct {
	x int
	y int
}

func (h *NodeHeap) Len() int {
	return len(h.slice)
}

func (h *NodeHeap) Less(i, j int) bool {
	// max heap inverts order
	return getScore(h.score, h.slice[i]) > getScore(h.score, h.slice[j])
}

func (h *NodeHeap) Swap(i, j int) {
	h.slice[i], h.slice[j] = h.slice[j], h.slice[i]
}

func (h *NodeHeap) Pop() any {
	item := h.slice[0]
	h.slice = h.slice[1:]
	delete(h.contents, item)
	return item
}

func (h *NodeHeap) Push(x any) {
	h.slice = append(h.slice, x.(int))
	h.contents[x.(int)] = true
}

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	input := make(chan int)
	output := make(chan int)

	go func() {
		intcode.ExecFull(program, input, output)
		panic("Program terminated - error")
	}()

	// I suppose I tell it what to do.
	// We'll go with A^* search as that seems to be a direct match for the
	// program statement.  Something dumber would probably work too.
	nodeCoords := make(map[int]node)
	reverseNodeCoords := make(map[node]int)

	startNode := 0
	// start is at 0, 0
	// other nodes will get added as necessary
	nodeCoords[startNode] = node{0, 0}
	reverseNodeCoords[node{0, 0}] = startNode

	// what's stored
	nodeContents := make(map[node]int)
	nodeContents[nodeCoords[0]] = HALLWAY

	parent := make(map[int]int)
	guessScore := make(map[int]int) // access using getScore()
	goalScore := make(map[int]int)  // access using getScore()

	openSet := &NodeHeap{
		score:    guessScore,
		contents: make(map[int]bool),
		slice:    make([]int, 0),
	}
	heap.Init(openSet)
	heap.Push(openSet, startNode)

	// for each node in the space, see which nodes it's connected to.
	currentNode := 0
	nodeNum := 0

	goalScore[startNode] = 0
	guessScore[startNode] = 1

	for openSet.Len() > 0 {
		PrintKnowledgeOfArea(nodeContents, reverseNodeCoords)
		nextNode := heap.Pop(openSet).(int)
		if nextNode != currentNode {
			// must travel from where we are to the current node.
			// this is similar to the orbit problem.  we traverse backwards
			// from both through the parent map and find the first point of
			// intersection.
			seen := make(map[int]bool)
			n := currentNode
			seen[n] = true

			for n != startNode {
				seen[n] = true
				n = parent[n]
			}
			n = nextNode
			path := make([]int, 0)
			for !seen[n] {
				// bad perf but I am too lazy to reverse a slice in golang.
				path = append([]int{n}, path...)
				n = parent[n]
			}
			intersectionPoint := n
			// must navigate from currentLocation to intersection point,
			// then from intersection point to node.
			for currentNode != intersectionPoint {
				prevNode := parent[currentNode]
				currentLocation := nodeCoords[currentNode]
				parentLocation := nodeCoords[prevNode]
				direction := getDirection(currentLocation, parentLocation)
				input <- direction
				result := <-output
				if result != 1 {
					panic("Invalid result navigating through known locations")
				}
				currentNode = prevNode
			}

			// currentNode is now intersection point.
			i := 0
			for currentNode != nextNode {
				currentLocation := nodeCoords[currentNode]
				stepNode := path[i]
				stepLocation := nodeCoords[stepNode]
				direction := getDirection(currentLocation, stepLocation)
				fmt.Println("[second] navigating in direction", direction, "from", currentLocation, "to", stepLocation)
				input <- direction
				result := <-output
				if result != 1 {
					log.Panicf("Invalid result navigating through known locations: %d", result)
				}
				currentNode = stepNode
				i++
			}
			// we should now be where we were looking to be.
		}
		// now current node is where we are.
		currentLocation := nodeCoords[currentNode]
		// the goal condition won't happen here as it will happen when we
		// detect neighbors of the current node.

		// now detect the neighbors.
		// our commands are north, south, west, east.
		// arranged in such a way.
		for n, offset := range [][2]int{{0, -1}, {0, 1}, {-1, 0}, {1, 0}} {
			direction := n + 1
			// for each direction, determine, do we have a node at that location?
			dx, dy := offset[0], offset[1]
			connection := node{currentLocation.x + dx, currentLocation.y + dy}
			fmt.Println("exploring", connection, "via direction", direction)
			switch nodeContents[connection] {
			case WALL:
				fmt.Println("but it is a wall so we should not go there")
				// on to the next direction
				continue
			case OXYGEN_SYSTEM:
				// this should never happen
				panic("Should not be able to see oxygen system as neighbor")
			case UNKNOWN:
				// we want to try to move to it, and create a new node.
				input <- direction
				result := <-output
				fmt.Println("output from moving in direction", direction, result, offset)
				switch result {
				case 0:
					// there's a wall.  our position has not changed.
					fmt.Println("discovered wall at", connection, "(moving", direction, ")")
					nodeContents[connection] = WALL
				case 2:
					// we need to stop when we hit this.  for now let's just
					// do nothing and add a print statement.
					fmt.Println("found the oxygen system!")
					steps := 0
					n = currentNode
					for n != startNode {
						n = parent[n]
						steps++
					}
					fmt.Println(steps + 1)
					return
				case 1:
					nodeContents[connection] = HALLWAY
					// add a new node at this location.
					nodeNum++
					nodeCoords[nodeNum] = connection
					reverseNodeCoords[connection] = nodeNum
					parent[nodeNum] = currentNode
					fmt.Printf("!! new node %d detected at (%d,%d)\n", nodeNum, connection.x, connection.y)

					fmt.Println("we are reversing our direction", reverseDirection((direction)))
					input <- reverseDirection(direction)
					reverseResult := <-output
					if reverseResult != 1 {
						panic("Unable to step back into previous node.  Should never happen.")
					}
				}
			}

			if nodeContents[connection] == HALLWAY {
				// OK, we want to understand the score
				fmt.Println("trying to understand if we should visit", connection)
				neighborNode := reverseNodeCoords[connection]
				tentativeScore := getScore(guessScore, currentNode) + 1
				if tentativeScore < getScore(guessScore, neighborNode) {
					parent[neighborNode] = currentNode
					goalScore[neighborNode] = tentativeScore
					// wikipedia adds a heuristic here, not clear we have one.
					// add one for now.
					guessScore[neighborNode] = tentativeScore + 1
					if !openSet.contents[neighborNode] {
						fmt.Println("adding", neighborNode, "to heap")
						heap.Push(openSet, neighborNode)
					}
				} else {
					fmt.Println("seems like no we should not")
				}
			}

		}
	}
	fmt.Println(openSet)
	// time to print out knowledge.
	PrintKnowledgeOfArea(nodeContents, reverseNodeCoords)
	panic("Failed to find oxygen system")
}
