package main

import (
	"bufio"
	"fmt"
	"os"
)

type snowEdge = struct {
	weight int
	source int
	dest   int
}

func toGraphViz(edges []snowEdge) {
	fmt.Println("digraph G {")
	for _, e := range edges {
		fmt.Printf("\t Node%d -> Node%d [label=\"%d\"];\n", e.source, e.dest, e.weight)
	}
	fmt.Println("}")
}

func day23(f *os.File) {
	rows := 0
	columns := 0
	startX := -1
	startY := 0
	endX := -1
	endY := -1
	grid := make([]byte, 0)

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		line := scanner.Text()
		for i, c := range line {
			if rows == 0 && c == '.' {
				startX = i
			}
		}
		grid = append(grid, line...)
		columns = len(line)
		rows++
	}

	for j := 0; j < columns; j++ {
		if grid[(rows-1)*columns+j] == '.' {
			endX = j
			endY = rows - 1
		}
	}

	// let's just make a graph.
	edges := make([]snowEdge, 0)

	nodeLocation := make([]int, len(grid))
	for i := range nodeLocation {
		nodeLocation[i] = -1
	}
	nodeLocation[startY*columns+startX] = 0
	nodeLocation[endY*columns+endX] = 1

	nodeNum := 1
	// down, up, right, left
	dirs := [][]int{{0, 1}, {0, -1}, {1, 0}, {-1, 0}}
	invalidCharForDir := []byte{'^', 'v', '<', '>'}
	validCharForDir := []byte{'v', '^', '>', '<'}

	// attempt to identify nodes, then start walking between nodes to connect
	// them.
	for x := 0; x < columns; x++ {
		for y := 0; y < rows; y++ {
			numValidDirs := 0
			if grid[y*columns+x] == '#' {
				continue
			}
			for _, d := range dirs {
				dx, dy := d[0], d[1]
				isValidY := y+dy < rows && y+dy >= 0
				isValidX := x+dx < columns && x+dx >= 0
				if isValidX && isValidY {
					g := grid[(y+dy)*columns+x+dx]
					if g != '#' && g != '.' {
						numValidDirs++
					}
				}
			}
			if numValidDirs > 1 {
				// new node here.
				nodeNum++
				nodeLocation[y*columns+x] = nodeNum
			}
		}
	}

	str := ""
	for i := 0; i < rows; i++ {
		line := ""
		for j := 0; j < columns; j++ {
			n := nodeLocation[i*columns+j]
			if n != -1 {
				line += fmt.Sprintf("%d", n)
			} else {
				line += string(grid[i*columns+j])
			}
		}
		str += line + "\n"
	}
	seenEdges := make(map[string]bool)

	queue := make([][4]int, 0)
	queue = append(queue, [4]int{0, startX, startY, 0})
	for len(queue) > 0 {
		item := queue[0]
		queue = queue[1:]
		node, x, y, distance := item[0], item[1], item[2], item[3]
		visited := make(map[int]bool)
		for {
			visited[y*columns+x] = true
			var validDirs [4]bool
			numValidDirs := 0
			for i, d := range dirs {
				dx, dy := d[0], d[1]
				isValidX := x+dx < columns && x+dx >= 0
				isValidY := y+dy < rows && y+dy >= 0

				if isValidX && isValidY &&
					!visited[(y+dy)*columns+(x+dx)] &&
					grid[(y+dy)*columns+x+dx] != '#' &&
					grid[(y+dy)*columns+x+dx] != invalidCharForDir[i] {
					numValidDirs++
					validDirs[i] = true
				}
			}

			n := nodeLocation[y*columns+x]
			if n != -1 && n != node {
				fmt.Println("connected walk to node", n)
				// now we find valid directions from here and push it onto the
				// queue.
				e := snowEdge{distance, node, n}
				key := fmt.Sprintf("%d-%d-%d", node, n, distance)
				if seenEdges[key] {
					// nothing to do
					break
				}
				seenEdges[key] = true
				edges = append(edges, e)

				for i := 0; i < 4; i++ {
					if validDirs[i] {
						dx, dy := dirs[i][0], dirs[i][1]
						if grid[((y+dy)*columns)+(x+dx)] != validCharForDir[i] {
							panic("Split did not happen on expected char")
						}
						if grid[(y+dy)*columns+x+dx] == invalidCharForDir[i] {
							panic("not sure we need to do this")
						}
						newItem := [4]int{n, x + dx*2, y + dy*2, 2}
						fmt.Println("queue pushing", newItem)
						queue = append(queue, newItem)
					}
				}
				break
			}

			if numValidDirs == 0 {
				fmt.Println(x, y, validDirs)
				panic("should not have gotten here")
			}

			if numValidDirs > 1 {
				panic("Should not have had more than one choice at a non-node location")
			}

			for i := 0; i < 4; i++ {
				if validDirs[i] {
					x += dirs[i][0]
					y += dirs[i][1]
					break
				}
			}
			distance++
		}
	}

	outgoingEdges := make(map[int][]snowEdge)
	for _, e := range edges {
		val, ok := outgoingEdges[e.source]
		if !ok {
			val = make([]snowEdge, 0)
		}
		val = append(val, e)
		outgoingEdges[e.source] = val
	}

	maxWeight := make(map[int]int)
	maxWeight[0] = 0
	maxQueue := make([][2]int, 0)
	maxQueue = append(maxQueue, [2]int{0, 0})
	for len(maxQueue) > 0 {
		item := maxQueue[0]
		node, cost := item[0], item[1]
		maxQueue = maxQueue[1:]

		for _, e := range outgoingEdges[node] {
			if e.weight+cost > maxWeight[e.dest] {
				maxWeight[e.dest] = e.weight + cost
				maxQueue = append(maxQueue, [2]int{e.dest, e.weight + cost})
			}
		}
	}
	fmt.Println(maxWeight[1])
}
