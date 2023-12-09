package main

import (
	"bufio"
	"fmt"
	"os"
)

func allZeroes(il []int) bool {
	for _, v := range il {
		if v != 0 {
			return false
		}
	}
	return true
}

func differences(il []int) []int {
	res := make([]int, len(il)-1)
	for i := 1; i < len(il); i++ {
		res[i-1] = il[i] - il[i-1]
	}
	return res
}

func computeMissing(il []int) int {
	if allZeroes(il) {
		return 0
	}
	diff := differences(il)
	n := computeMissing(diff)
	return il[0] - n
}

func day9(f *os.File) {
	scanner := bufio.NewScanner(f)

	sum := 0
	for scanner.Scan() {
		line := scanner.Text()
		nums := extractFields(line)
		sum += computeMissing(nums)
	}
	fmt.Println(sum)
}
