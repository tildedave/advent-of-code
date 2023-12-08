package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type Node = struct {
	left  string
	right string
}

func isDone(nodes []string) bool {
	for _, s := range nodes {
		if s[len(s)-1] != 'Z' {
			return false
		}
	}
	return true
}

func day8(f *os.File) {
	scanner := bufio.NewScanner(f)

	i := 0
	var instructions string
	nodeMap := make(map[string]Node, 0)
	for scanner.Scan() {
		line := scanner.Text()
		if i == 0 {
			// first line, instructions go here
			instructions = strings.TrimSpace(line)
		}
		if strings.Contains(line, "=") {
			res := strings.Split(line, " = ")
			left := strings.TrimSpace(res[0])
			right := strings.Split(strings.ReplaceAll(strings.ReplaceAll(res[1], "(", ""), ")", ""), ", ")
			nodeMap[left] = Node{left: right[0], right: right[1]}
		}
		i++
	}

	var nodes []string = make([]string, 0)
	for k := range nodeMap {
		if k[len(k)-1] == 'A' {
			nodes = append(nodes, k)
		}
	}
	steps := 0
	// For each of the starting nodes we are looking for a loop.  A loop is when
	// we get to a point in the steps where we have been before and we are about
	// to execute the same point of the instructions.
	// Then we do some calculation with the loops to figure out when we would end.
	for _, node := range nodes {
		m := make(map[string]int)
		k := 0
		j := 0
		for {
			dir := instructions[j]
			key := fmt.Sprintf("%s-%d", node, j)
			if m[key] != 0 {
				// we have been here exactly here before, loop!
				// question: when did the loop start?
				// answer: the loop started at the first thing we saw.
				startLoop, err := strconv.ParseInt(strings.Split(key, "-")[1], 10, 64)
				if err != nil {
					log.Fatal(err)
				}
				fmt.Println("loop", key, k, startLoop)
				for n := range m {
					node := strings.Split(n, "-")[0]
					if node[len(node)-1] == 'Z' {
						fmt.Println(n, m[n])
					}
				}

				break
			}
			m[key] = k
			if dir == 'L' {
				node = nodeMap[node].left
			} else if dir == 'R' {
				node = nodeMap[node].right
			} else {
				panic("Invalid")
			}
			j = (j + 1) % len(instructions)
			steps++
			k++
		}
	}
	// we have to sync all the loops up with each other, and then determine
	// when they will all hit a terminal state 'Z' at the end
	// I actually solved this by getting Sage to compute the least common
	// multiple of each of the loop lengths.
	fmt.Println(steps)
}
