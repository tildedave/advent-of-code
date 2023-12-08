package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type Node = struct {
	left  string
	right string
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
			instructions = line
		}
		if strings.Contains(line, "=") {
			res := strings.Split(line, " = ")
			left := strings.TrimSpace(res[0])
			right := strings.Split(strings.ReplaceAll(strings.ReplaceAll(res[1], "(", ""), ")", ""), ", ")
			nodeMap[left] = Node{left: right[0], right: right[1]}
		}
		i++
	}

	node := "AAA"
	j := 0
	steps := 0
	for node != "ZZZ" {
		dir := instructions[j]
		if dir == 'L' {
			node = nodeMap[node].left
		} else if dir == 'R' {
			node = nodeMap[node].right
		} else {
			panic("Invalid")
		}
		j = (j + 1) % len(instructions)
		steps++
	}
	fmt.Println(steps)
}
