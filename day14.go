package main

import (
	"bufio"
	"fmt"
	"math/bits"
	"os"
)

func day14(f *os.File) {
	scanner := bufio.NewScanner(f)
	grid := [][]byte{}
	i := 0
	for scanner.Scan() {
		line := scanner.Text()
		row := make([]byte, len(line))
		for j, c := range line {
			row[j] = byte(c)
		}
		grid = append(grid, row)
		i++
	}
	// go upwards and split by '#'s
	// 0 = empty, 1 = O
	results := [][]int{}
	for j := 0; j < len(grid[0]); j++ {
		curr := uint(0)
		for i := len(grid) - 1; i >= 0; i-- {
			c := grid[i][j]
			// fmt.Println(string(c))
			if c == '#' {
				// break, push to next
				if curr != 0 {
					results = append(results, []int{bits.OnesCount(curr), i + 1})
				}
				curr = 0
			} else if c == 'O' {
				curr = (curr << 1) | 1
			} else if c == '.' {
				curr = curr << 1
			}
		}
		if curr != 0 {
			results = append(results, []int{bits.OnesCount(curr), 0})
		}
	}
	score := 0
	for _, r := range results {
		oc, idx := r[0], r[1]
		adder := len(grid) - idx
		for s := 0; s < oc; s++ {
			score += adder
			adder -= 1
		}
	}
	fmt.Println(score)
}
