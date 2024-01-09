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

func toGraphViz(allLabels map[string]bool, dist map[string]map[string]int) {
	// I guess let's see what graphviz claims.
	labelMap := make(map[string]int)
	n := 0
	fmt.Print("digraph G {\n")
	for e := range allLabels {
		labelMap[e] = n
		fmt.Printf("\tNode%d[label=\"%s\"];\n", n, e)
		n++
	}
	for k, dists := range dist {
		for j, dist := range dists {
			fmt.Printf("\tNode%d -> Node%d [label=\"%d\"];\n", labelMap[j], labelMap[k], dist)
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
	fmt.Println(startPos)

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
	fmt.Println(edges)

	allLabels := make(map[string]bool)

	for _, elist := range edges {
		for _, e := range elist {
			allLabels[e.source] = true
			allLabels[e.dest] = true
		}
	}

	// Floyd Warshall now I guess
	// dist := make(map[string]map[string]int)
	// for label := range allLabels {
	// 	dist[label] = make(map[string]int)
	// 	for label2 := range allLabels {
	// 		// just needs to be very large, math.maxInt by itself overflows
	// 		// immediately so we don't want that.
	// 		dist[label][label2] = (math.MaxInt >> 2)
	// 	}
	// 	dist[label][label] = 0
	// }

	// for _, elist := range edges {
	// 	for _, e := range elist {
	// 		dist[e.source][e.dest] = e.weight
	// 		dist[e.dest][e.source] = e.weight
	// 	}
	// }
	// for k := range allLabels {
	// 	for i := range allLabels {
	// 		for j := range allLabels {
	// 			if dist[i][j] > dist[i][k]+dist[k][j] {
	// 				dist[i][j] = dist[i][k] + dist[k][j]
	// 			}
	// 		}
	// 	}
	// }
	// fmt.Println(dist)

	nums := make(map[string]uint)
	num := uint(0)
	for e := range allLabels {
		nums[e] = num
		num++
	}

	// OK this is our graph.  we now want a minimum walk starting from @ to
	// every node with the requirement that we hit the lowercase before the
	// capitals.

	// I'm lazy, let's try a combinatorial search.  how bad is this?
	type searchItem struct {
		node   string
		length int
		seen   bitset.BitSet
		path   string
	}
	searchQueue := make([]searchItem, 0)
	searchQueue = append(searchQueue, searchItem{"@", 0, bitset.BitSet{}, ""})
	minDistance := math.MaxInt

	// actually we only need to get every key.  going everywhere is potentially
	// unnecessary.
	for len(searchQueue) > 0 {
		item := searchQueue[0]
		searchQueue = searchQueue[1:]
		fmt.Println("I am at", item.node)
		hadValidLabel := false
		item.seen.Set(nums[item.node])
		path := item.path + item.node

		fmt.Println(edges[item.node])
		for _, e := range edges[item.node] {
			// is this a valid label?
			label := e.dest
			n := nums[label]
			// unfortunately we do want people to be able to return to @ in the
			// path, I guess as often as they like.
			// this seems OK in the examples I see.
			if item.seen.Test(n) && label != "@" {
				fmt.Println("I have already seen ", label, "since my path so far is", path)
				continue
			}

			// next, determine if we need to have a key, and if we do have a key.
			if isUppercase(label) {
				// must have the key, we may not have the key.
				req := nums[strings.ToLower(label)]
				if !item.seen.Test(req) {
					fmt.Println("I cannot go to ", label, "since my path so far is", path)
					continue
				}
			}

			// this is a valid label to connect to.
			nextSeen := bitset.BitSet{}
			for i, e := item.seen.NextSet(0); e; i, e = item.seen.NextSet(i + 1) {
				nextSeen.Set(i)
			}
			fmt.Println("connect to ", label, "through", item.node)
			nextItem := searchItem{label, item.length + e.weight, nextSeen, path}
			searchQueue = append(searchQueue, nextItem)
			hadValidLabel = true
		}
		if !hadValidLabel {
			if item.length < minDistance {
				fmt.Println("min item found", item)
				minDistance = item.length
			}
		}
	}
	fmt.Println(minDistance)
	fmt.Println(edges)
}
