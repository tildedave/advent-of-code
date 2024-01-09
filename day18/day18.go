package day18

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strings"

	"github.com/bits-and-blooms/bitset"
)

const NORTH = 0
const EAST = 1
const SOUTH = 2
const WEST = 3

type coord struct {
	x int
	y int
}

type edge struct {
	source string
	dest   string
	weight int
}

func isLowercase(s string) bool {
	i := int(s[0])
	return i >= 97 && i <= 122
}

func isUppercase(s string) bool {
	i := int(s[0])
	return i >= 65 && i <= 90
}

func moveInDirection(pos coord, direction int) coord {
	dx, dy := 0, 0
	switch direction {
	case NORTH:
		dy -= 1
	case SOUTH:
		dy += 1
	case EAST:
		dx += 1
	case WEST:
		dx -= 1
	}
	return coord{pos.x + dx, pos.y + dy}
}

type queueItem struct {
	source string
	pos    coord
	length int
}

func toGraphViz(allLabels map[string]bool, edges map[string][]edge) {
	// I guess let's see what graphviz claims.
	labelMap := make(map[string]int)
	n := 0
	fmt.Print("digraph G {\n")
	for e := range allLabels {
		labelMap[e] = n
		fmt.Printf("\tNode%d[label=\"%s\"];\n", n, e)
		n++
	}
	for _, elist := range edges {
		for _, e := range elist {
			fmt.Printf("\tNode%d -> Node%d [label=\"%d\"];\n", labelMap[e.source], labelMap[e.dest], e.weight)
		}
	}
	fmt.Println("}")
}

var startSymbols = []string{"@", "$", "%", "&"}

func Run(f *os.File, partTwo bool) {
	scanner := bufio.NewScanner(f)
	grid := make([][]byte, 0)
	row := 0
	var startPos coord
	for scanner.Scan() {
		line := scanner.Text()
		idx := strings.Index(line, "@")
		if idx != -1 {
			startPos = coord{idx, row}
		}
		strLine := make([]byte, len(line))
		for n := range line {
			strLine[n] = line[n]
		}
		grid = append(grid, strLine)
		row++
	}

	starts := make([]coord, 0)
	if partTwo {
		for _, dx := range []int{-1, 1} {
			for _, dy := range []int{-1, 1} {
				grid[startPos.y+dy][startPos.x+dx] = startSymbols[len(starts)][0]
				starts = append(starts, coord{startPos.x + dx, startPos.y + dy})
			}
		}
		for _, dx := range []int{-1, 1} {
			grid[startPos.y][startPos.x+dx] = '#'
		}
		for _, dy := range []int{-1, 1} {
			grid[startPos.y+dy][startPos.x] = '#'
		}
		grid[startPos.y][startPos.x] = '#'
	}

	for _, line := range grid {
		for _, ch := range line {
			fmt.Print(string(ch))
		}
		fmt.Println()
	}

	visited := make(map[string]map[coord]bool)
	queue := make([]queueItem, 0)
	for n, pos := range starts {
		queue = append(queue, queueItem{startSymbols[n], pos, 0})
	}
	edges := make(map[string][]edge)
	for len(queue) > 0 {
		item := queue[0]
		queue = queue[1:]

		source := item.source
		length := item.length

		if visited[item.source] == nil {
			visited[item.source] = make(map[coord]bool)
		}
		visited[item.source][item.pos] = true
		curr := grid[item.pos.y][item.pos.x]
		if item.length > 0 {
			if string(curr) == source {
				// we discovered ourselves.  great.
				continue
			}

			if curr != '#' && curr != '.' {
				// we have an edge!
				source = string(curr)

				// note: if source is a
				length = 0

				edgeList := edges[item.source]
				if edgeList == nil {
					edgeList = make([]edge, 0)
				}
				edgeList = append(edgeList, edge{source: item.source, dest: source, weight: item.length})
				edges[item.source] = edgeList

				hasReverse := false
				edgeList = edges[source]
				if edgeList == nil {
					edgeList = make([]edge, 0)
				}
				for _, e := range edgeList {
					if e.source == item.source {
						hasReverse = true
					}
				}
				if !hasReverse {
					edgeList = append(edgeList, edge{source: source, dest: item.source, weight: item.length})
				}
				edges[source] = edgeList
			}
		}

		// walk from our position.
		// in the event that we have more than one direction to go, put it on
		// the queue.
		// our visited queue needs to be based on the source.

		pos := item.pos
		for _, dir := range []int{NORTH, SOUTH, EAST, WEST} {
			next := moveInDirection(pos, dir)
			ch := grid[next.y][next.x]
			if ch != '#' && !visited[source][next] {
				queue = append(queue, queueItem{source, next, length + 1})
			}
		}
	}

	newEdges := make(map[string][]edge)
	for node, elist := range edges {
		newList := make([]edge, 0)
		minNeighbors := make(map[string]edge)
		for _, e := range elist {
			if minNeighbors[e.dest].weight == 0 || e.weight < minNeighbors[e.dest].weight {
				minNeighbors[e.dest] = e
			}
		}
		for _, edge := range minNeighbors {
			newList = append(newList, edge)
		}
		newEdges[node] = newList
	}
	edges = newEdges
	fmt.Println(edges)

	allLabels := make(map[string]bool)

	for _, elist := range edges {
		for _, e := range elist {
			allLabels[e.source] = true
			allLabels[e.dest] = true
		}
	}

	nums := make(map[string]uint)
	num := uint(0)
	numKeys := uint(0)
	for e := range allLabels {
		if isLowercase(e) {
			nums[e] = num
			numKeys++
		}
		num++
	}
	labelNums := make(map[uint]string)
	for k, v := range nums {
		labelNums[v] = k
	}
	for e := range allLabels {
		_, ok := nums[e]
		if !ok {
			nums[e] = num
		}
		num++
	}

	// OK this is our graph.  we now want a minimum walk starting from @ to
	// every node with the requirement that we hit the lowercase before the
	// capitals.

	type searchItem struct {
		node   []string
		length int
		keys   *bitset.BitSet
		path   string
		uniq   int
	}
	searchQueue := make([]searchItem, 0)
	startNodes := make([]string, len(starts))
	for n := range starts {
		startNodes[n] = startSymbols[n]
	}
	searchQueue = append(searchQueue, searchItem{startNodes, 0, bitset.New(numKeys), "", 0})

	type bitsetCost = struct {
		b    *bitset.BitSet
		cost int
	}
	minDistance := make(map[string][]bitsetCost)
	minTotal := math.MaxInt

	everyKey := bitset.New(numKeys)
	for e := range allLabels {
		if isLowercase(e) {
			everyKey.Set(nums[e])
		}
	}

	allKeys := make([]*bitset.BitSet, len(starts))
	// DFS from each starting position to find which keys are available?
	for n, start := range startSymbols {
		visited := make(map[string]bool)
		q := make([]string, 0)
		q = append(q, start)
		bs := bitset.New(numKeys)
		for len(q) > 0 {
			i := q[0]
			q = q[1:]
			visited[i] = true
			for _, e := range edges[i] {
				if !visited[e.dest] {
					q = append(q, e.dest)
				}
			}
		}
		for v := range visited {
			if isLowercase(v) {
				bs.Set(nums[v])
			}
		}
		allKeys[n] = bs
	}

	keyString := func(b *bitset.BitSet) string {
		str := ""
		for i, e := b.NextSet(0); e; i, e = b.NextSet(i + 1) {
			str += labelNums[i]
		}
		return str
	}
	for n, keys := range allKeys {
		fmt.Println(n, keyString(keys))
	}

	nodeString := func(nstr []string) string {
		str := ""
		for _, s := range nstr {
			str += s
		}
		return str
	}

	itemToString := func(i *searchItem) string {
		return fmt.Sprintf("%s %s %d %s uniq%d", nodeString(i.node), keyString(i.keys), i.length, i.path, i.uniq)
	}
	fmt.Println(itemToString(&searchQueue[0]))
	itemCount := 1

	// actually we only need to get every key.  going everywhere is potentially
	// unnecessary.
	for len(searchQueue) > 0 {
		item := searchQueue[0]
		searchQueue = searchQueue[1:]
		nodeStr := nodeString(item.node)

		// fmt.Println("---> Queue", itemToString(&item), len(searchQueue))

		if minDistance[nodeStr] == nil {
			minDistance[nodeStr] = make([]bitsetCost, 0)
		}

		path := item.path

		shouldCutoff := false
		for _, bc := range minDistance[nodeStr] {
			if bc.b.IsSuperSet(item.keys) && bc.cost <= item.length {
				shouldCutoff = true
			}
		}
		if shouldCutoff {
			continue
		} else {
			minDistance[nodeStr] = append(minDistance[nodeStr], bitsetCost{item.keys, item.length})
		}

		if item.keys.Equal(everyKey) {
			if item.length < minTotal {
				fmt.Println("found all key path", path, item.length)
				minTotal = item.length
			}
			continue
		}

		nextNodes := make([]string, len(item.node))
		copy(nextNodes, item.node)
		for n, node := range item.node {
			if item.keys.IsSuperSet(allKeys[n]) {
				continue
			}
			for _, e := range edges[node] {
				next := e.dest
				// fmt.Println("inspecting edge from ", node, "to", next)

				// next, determine if we need to have a key, and if we do have a key.
				if isUppercase(next) {
					// must have the key, we may not have the key.
					req := nums[strings.ToLower(next)]
					if !item.keys.Test(req) {
						// fmt.Println("I can't go to", next, "because I don't have the key", keyString(item.keys), path)
						continue
					}
				}

				nextNodes[n] = next
				nextStr := nodeString(nextNodes)

				nextKeys := bitset.New(numKeys)
				for i, e := item.keys.NextSet(0); e; i, e = item.keys.NextSet(i + 1) {
					nextKeys.Set(i)
				}
				if isLowercase(next) {
					// acquire the key.  this will sometimes be redundant.
					nextKeys.Set(nums[next])
				}

				// we can walk here.  however it might not be the fastest path.
				// must look up minDistance.
				costs := minDistance[nextStr]
				shouldCutoff = false
				for _, c := range costs {
					if c.b.IsSuperSet(nextKeys) && c.cost <= item.length+e.weight {
						shouldCutoff = true
						break
					}
				}
				if shouldCutoff {
					continue
				}

				itemCount++
				copyNextNodes := make([]string, len(nextNodes))
				copy(copyNextNodes, nextNodes)
				nextItem := searchItem{copyNextNodes, item.length + e.weight, nextKeys, path + next, itemCount}
				// fmt.Println("walking", nextNodes, "length", nextItem.length, "with keys", nextKeys.String(), "new path", path+next, "uniq", nextItem.uniq)
				searchQueue = append(searchQueue, nextItem)
			}
			nextNodes[n] = node
		}
	}
	fmt.Println(minTotal)
}
