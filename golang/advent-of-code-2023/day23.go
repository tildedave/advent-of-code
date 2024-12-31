package main

import (
	"bufio"
	"fmt"
	"os"

	"github.com/bits-and-blooms/bitset"
)

type snowEdge = struct {
	weight int
	source uint
	dest   uint
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
	// validCharForDir := []byte{'v', '^', '>', '<'}

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

	queue := make([][6]int, 0)
	queue = append(queue, [6]int{0, startX, startY, 0, -1, -1})
	for len(queue) > 0 {
		item := queue[0]
		queue = queue[1:]
		node, x, y, distance, visitedX, visitedY := item[0], item[1], item[2], item[3], item[4], item[5]
		visited := make(map[int]bool)
		// kludge to prevent stepping backwards
		if visitedX != -1 && visitedY != -1 {
			visited[visitedY*columns+visitedX] = true
		}
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
					grid[(y+dy)*columns+x+dx] != '#' {
					numValidDirs++
					validDirs[i] = true
				}
			}

			n := nodeLocation[y*columns+x]
			if n != -1 && n != node {
				// now we find valid directions from here and push it onto the
				// queue.
				e := snowEdge{distance, uint(node), uint(n)}
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
						// if grid[((y+dy)*columns)+(x+dx)] != validCharForDir[i] {
						// 	panic("Split did not happen on expected char")
						// }
						newItem := [6]int{n, x + dx*2, y + dy*2, 2, x + dx, y + dy}
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
				fmt.Println(x, y, validDirs)
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

	outgoingEdges := make(map[uint][]snowEdge)
	for _, e := range edges {
		val, ok := outgoingEdges[e.source]
		if !ok {
			val = make([]snowEdge, 0)
		}
		val = append(val, e)
		outgoingEdges[e.source] = val
	}

	// combinatorial algorithm time I suppose.
	type queueItem = struct {
		node      uint
		seenNodes bitset.BitSet
		cost      int
	}
	maxQueue := make([]queueItem, 0)
	maxQueue = append(maxQueue, queueItem{0, bitset.BitSet{}, 0})
	maxCost := 0

	for len(maxQueue) > 0 {
		item := maxQueue[0]
		maxQueue = maxQueue[1:]

		item.seenNodes.Set(item.node)

		if item.node == uint(1) {
			if item.cost > maxCost {
				maxCost = item.cost
			}
			continue
		}

		hasFinalOutput := false
		for _, e := range outgoingEdges[item.node] {
			if e.dest == 1 {
				newItem := queueItem{e.dest, bitset.BitSet{}, e.weight + item.cost}
				maxQueue = append(maxQueue, newItem)
				hasFinalOutput = true
				break
			}
		}

		if !hasFinalOutput {
			for _, e := range outgoingEdges[item.node] {
				if !item.seenNodes.Test(e.dest) {
					var newSeenNodes bitset.BitSet
					// This ends up being faster than CopyFull() in practice for whatever reason.
					for i, e := item.seenNodes.NextSet(0); e; i, e = item.seenNodes.NextSet(i + 1) {
						newSeenNodes.Set(i)
					}
					if item.seenNodes.Count() != newSeenNodes.Count() {
						panic("failed")
					}

					newItem := queueItem{e.dest, newSeenNodes, e.weight + item.cost}
					maxQueue = append(maxQueue, newItem)
				}
			}
		}
	}
	fmt.Println(maxCost)
}
