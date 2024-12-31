package main

import (
	"bufio"
	"fmt"
	"math"
	"math/rand"
	"os"
	"strings"
)

func edgeKey(source string, dest string) string {
	return fmt.Sprintf("%s|%s", source, dest)
}

func dfsPath(
	adjacency map[string][]string,
	flow map[string]int,
	source string,
	dest string,
) ([]string, map[string]bool, bool) {
	// dfs from source to dest
	visited := make(map[string]bool)
	queued := make(map[string]bool)

	parents := make(map[string]string)
	queue := make([]string, 0)
	queue = append(queue, source)
	pathExists := false
	for len(queue) > 0 {
		// look at the edges of curr
		node := queue[0]
		queue = queue[1:]
		visited[node] = true
		if node == dest {
			// done, parent graph will be enough to reconstruct
			pathExists = true
			break
		}
		for _, e := range adjacency[node] {
			// can't have max flow on the network, max flow is one.
			if !visited[e] && !queued[e] && 1-flow[edgeKey(node, e)] > 0 {
				queue = append(queue, e)
				parents[e] = node
				queued[e] = true
			}
		}
	}
	if !pathExists {
		return []string{}, visited, pathExists
	}

	reversePath := make([]string, 0)
	reversePath = append(reversePath, dest)
	curr := dest
	for curr != source {
		curr = parents[curr]
		reversePath = append(reversePath, curr)
	}
	// path is actually in reverse now.
	path := make([]string, len(reversePath))
	for i := range reversePath {
		path[len(path)-1-i] = reversePath[i]
	}

	return path, visited, pathExists
}

func fordFulkerson(adjacency map[string][]string, source string, dest string) bool {
	// our cost = 1 here
	flow := make(map[string]int)
	numIterations := 0
	for {
		path, visited, exists := dfsPath(adjacency, flow, source, dest)
		numIterations++
		if !exists {
			unvisited := make(map[string]bool)
			numEdges := 0
			for k := range adjacency {
				if !visited[k] {
					unvisited[k] = true
				}
				for _, d := range adjacency[k] {
					if visited[k] && !visited[d] {
						numEdges++
					}
				}
			}
			if numEdges == 3 {
				fmt.Println(len(visited) * len(unvisited))
				return true
			}
			return false
		}
		minCapacity := math.MaxInt
		for i, curr := range path {
			if i == 0 {
				continue
			}
			cfuv := 1 - flow[edgeKey(path[i-1], curr)]
			if cfuv < minCapacity {
				minCapacity = cfuv
			}
		}
		fmt.Println("min capacity", minCapacity)
		if minCapacity == math.MaxInt {
			fmt.Println(path)
			panic("should not have done this")
		}
		// path is 1 since we only have edge weights of 1
		for i, curr := range path {
			if i == 0 {
				continue
			}
			prev := path[i-1]
			key := edgeKey(prev, curr)
			reverseKey := edgeKey(curr, prev)
			flow[key] += minCapacity
			flow[reverseKey] -= minCapacity
		}
	}
}

func visualize(adjacency map[string][]string) {
	fmt.Println("digraph G {")
	for k, adj := range adjacency {
		for _, a := range adj {
			fmt.Printf("%s -> %s;\n", k, a)
		}
	}
	fmt.Println("}")
}

func day25(f *os.File) {
	scanner := bufio.NewScanner(f)
	adjacency := make(map[string][]string)
	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, ":")
		connections := strings.Fields(res[1])
		val, ok := adjacency[res[0]]
		if !ok {
			val = make([]string, 0)
		}
		val = append(val, connections...)
		adjacency[res[0]] = val

		for _, v := range connections {
			val, ok = adjacency[v]
			if !ok {
				val = make([]string, 0)
			}
			val = append(val, res[0])
			adjacency[v] = val
		}
	}

	keys := make([]string, 0)
	for k := range adjacency {
		keys = append(keys, k)
	}
	for {
		i := rand.Intn(len(adjacency))
		j := rand.Intn(len(adjacency))
		if i == j {
			continue
		}

		if fordFulkerson(adjacency, keys[i], keys[j]) {
			return
		}
	}
}
