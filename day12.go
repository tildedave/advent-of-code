package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

const OPERATIONAL = 0
const DAMAGED = 1
const UNKNOWN_STATE = 2

func parseConfiguration(s string) []int {
	ret := make([]int, 0)
	for _, i := range s {
		var c int
		switch i {
		case '#':
			c = DAMAGED
		case '.':
			c = OPERATIONAL
		case '?':
			c = UNKNOWN_STATE
		default:
			panic("Invalid character")
		}
		ret = append(ret, c)
	}
	return ret
}

func parseNumbers(slist []string) []int {
	ret := make([]int, 0)
	for _, s := range slist {
		i, err := strconv.ParseInt(s, 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		ret = append(ret, int(i))
	}
	return ret
}

func isConsistent(configuration []int, damageReport []int) bool {
	// can do this recursively
	// assumption: damageReport is all positive integers, no zeroes.
	// assumption: configuration has no "unknowns" in it.
	// -- can we weaken this?  let's try.
	j := 0
	if len(damageReport) == 0 {
		// this implies the rest of the configuration is operational
		for _, k := range configuration {
			// unknowns are OK because we can force them to OPERATIONAL.
			// can be k == DAMAGED
			if k != OPERATIONAL {
				return false
			}
		}
		return true
	}

	i := damageReport[0]
	for j < len(configuration) && configuration[j] == OPERATIONAL {
		j++
	}
	// now we're at the start of a damaged, or unknown block.
	// we need it to be the size that we claimed it is.
	if i > len(configuration)-j {
		return false
	}
	// now, we need it to be EXACTLY the size we want, and each entry needs to
	// be either damaged or unknown.
	for _, d := range configuration[j : j+i] {
		// can be d != OPERATIONAL
		if d != DAMAGED {
			return false
		}
	}

	// Now we're at the end of the block, and we need to either be at the end
	// of the string, or we need the damaged block to end, either through an
	// operational node or an unknown node.
	if j+i == len(configuration) || configuration[j+i] == OPERATIONAL {
		return isConsistent(configuration[j+i:], damageReport[1:])
	} else {
		return false
	}
}

func generateConfigurations(baseConfiguration []int) [][]int {
	ret := make([][]int, 0)

	c := baseConfiguration[0]
	if len(baseConfiguration) == 1 {
		if c == UNKNOWN_STATE {
			return [][]int{{OPERATIONAL}, {DAMAGED}}
		}
		return [][]int{{c}}
	}

	if c == UNKNOWN_STATE {
		recurVersion := make([]int, len(baseConfiguration)-1)
		copy(recurVersion, baseConfiguration[1:])
		for _, r := range generateConfigurations(recurVersion) {
			ret = append(ret, append([]int{OPERATIONAL}, r...))
			ret = append(ret, append([]int{DAMAGED}, r...))
		}
		return ret
	}

	for _, r := range generateConfigurations(baseConfiguration[1:]) {
		ret = append(ret, append([]int{c}, r...))
	}

	return ret
}

func day12(f *os.File) {
	scanner := bufio.NewScanner(f)
	total := 0
	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, " ")
		status := parseConfiguration(res[0])
		damageReport := parseNumbers(strings.Split(res[1], ","))
		lineTotal := 0
		for _, c := range generateConfigurations(status) {
			if isConsistent(c, damageReport) {
				lineTotal += 1
				total += 1
			}
		}
	}
	fmt.Println(total)
}
