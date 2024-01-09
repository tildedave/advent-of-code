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

func Run(f *os.File, partTwo bool) {
	scanner := bufio.NewScanner(f)
	grid := make([]string, 0)
	row := 0
	var startPos coord
	for scanner.Scan() {
		line := scanner.Text()
		idx := strings.Index(line, "@")
		if idx != -1 {
			startPos = coord{idx, row}
		}
		grid = append(grid, line)
		row++
	}

	visited := make(map[string]map[coord]bool)
	queue := make([]queueItem, 0)
	queue = append(queue, queueItem{"@", startPos, 0})
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
			if ch != '#' && ch != '@' && !visited[source][next] {
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
		node   string
		length int
		keys   *bitset.BitSet
		path   string
	}
	searchQueue := make([]searchItem, 0)
	searchQueue = append(searchQueue, searchItem{"@", 0, bitset.New(numKeys), ""})

	allKeys := bitset.New(numKeys)
	for label := range allLabels {
		if isLowercase(label) {
			allKeys.Set(nums[label])
		}
	}
	minDistance := make(map[string]map[string]int)
	minTotal := math.MaxInt

	// actually we only need to get every key.  going everywhere is potentially
	// unnecessary.
	for len(searchQueue) > 0 {
		item := searchQueue[0]
		searchQueue = searchQueue[1:]

		if minDistance[item.node] == nil {
			minDistance[item.node] = make(map[string]int)
		}
		if isLowercase(item.node) {
			// acquire the key
			item.keys.Set(nums[item.node])
		}

		keyStr := item.keys.String()
		path := item.path + item.node

		dist, ok := minDistance[item.node][keyStr]
		if !ok || item.length < dist {
			minDistance[item.node][keyStr] = item.length
		}
		if ok && dist >= item.length {
			// if strings.HasPrefix(path, "@a@f") {
			// 	fmt.Println("bailing out, we have a more efficient way to get to", item.node, path, dist, item.keys)
			// }
			continue
		}

		if item.keys.Equal(allKeys) {
			if item.length < minTotal {
				fmt.Println("found all key path", path, item.length)
				minTotal = item.length
			}
			continue
		}

		for _, e := range edges[item.node] {
			// is this a valid label?
			next := e.dest
			// n := nums[next]
			// unfortunately we do want people to be able to return to @ in the
			// path, I guess as often as they like.
			// this seems OK in the examples I see.
			// if isLowercase(next) && item.keys.Test(n) {
			// 	fmt.Println("I have already seen ", next, "since my path so far is", path)
			// 	continue
			// }

			// next, determine if we need to have a key, and if we do have a key.
			if isUppercase(next) {
				// must have the key, we may not have the key.
				req := nums[strings.ToLower(next)]
				if !item.keys.Test(req) {
					continue
				}
			}

			// we can walk here.  however it might not be the fastest path.
			dist, ok := minDistance[next][keyStr]
			if ok && dist < item.length+e.weight {
				continue
			}

			nextKeys := bitset.New(numKeys)
			for i, e := item.keys.NextSet(0); e; i, e = item.keys.NextSet(i + 1) {
				nextKeys.Set(i)
			}
			nextItem := searchItem{next, item.length + e.weight, nextKeys, path}
			searchQueue = append(searchQueue, nextItem)
		}
	}
	fmt.Println(minTotal)
}
