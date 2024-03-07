package day6

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

func Run(f *os.File, partTwo bool) {
	scanner := bufio.NewScanner(f)
	adjacency := make(map[string][]string)
	parent := make(map[string]string)

	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, ")")
		lhs, rhs := res[0], res[1]
		val, ok := adjacency[lhs]
		if !ok {
			val = make([]string, 0)
		}
		val = append(val, rhs)
		adjacency[lhs] = val
		parent[rhs] = lhs
		_, ok = adjacency[rhs]
		if !ok {
			adjacency[rhs] = make([]string, 0)
		}
	}

	if !partTwo {
		count := make(map[string]int)
		for n := range adjacency {
			// starting from n go upwards in the graph
			// base case: no parent.  If no parent, count is 0.
			// otherwise walk until you find the parent or a node where you
			// know the count.
			// once you get there, count number of steps, write its known
			// value, then write for each of its parent that didn't have the count.
			steps := 0
			currentNode := n
			knownCount := 0
			for {
				p, ok := parent[currentNode]
				if !ok {
					break
				}
				steps++
				knownCount, ok = count[p]
				if ok {
					break
				}
				currentNode = p
			}
			// now do the walk again with the known amount
			count[n] = steps + knownCount
			steps--
			for {
				p, ok := parent[currentNode]
				if !ok {
					break
				}
				_, ok = count[p]
				if ok {
					break
				}
				count[currentNode] = steps
				steps--
			}
		}
		total := 0
		for _, v := range count {
			total += v
		}
		fmt.Println(total)
		return
	}

	// part 2
	// we walk upward from YOU / SAN once, marking nodes we see along the way
	// after we do one walk we do walk from the other, until we find the root
	// node.
	// then the answer is distance from YOU to root + distance from SAN to root - 2.
	seen := make(map[string]bool)
	n := "SAN"
	ok := true
	for ok {
		seen[n] = true
		n, ok = parent[n]
	}
	n = "YOU"
	for !seen[n] {
		n = parent[n]
	}
	meetingNode := n
	distance := 0
	n = "SAN"
	for n != meetingNode {
		n = parent[n]
		distance++
	}
	n = "YOU"
	for n != meetingNode {
		n = parent[n]
		distance++
	}
	fmt.Println(distance - 2)
}
