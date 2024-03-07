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
		i, err := strconv.ParseInt(strings.TrimSpace(s), 10, 64)
		if err != nil {
			log.Fatal(err)
		}
		ret = append(ret, int(i))
	}
	return ret
}

func formatConfiguration(configuration []int) string {
	ret := ""
	for _, c := range configuration {
		if c == UNKNOWN_STATE {
			ret += "?"
		}
		if c == OPERATIONAL {
			ret += "."
		}
		if c == DAMAGED {
			ret += "#"
		}
	}

	return ret
}

func formatDamageReport(r []int) string {
	ret := ""
	for i, r := range r {
		if i != 0 {
			ret += ","
		}
		ret += fmt.Sprintf("%d", r)
	}
	return ret
}

func storeMemoizedResults(memoizedResults *map[string]int, key string, restCount int) int {
	if restCount == 0 {
		(*memoizedResults)[key] = -1
	} else {
		(*memoizedResults)[key] = restCount
	}
	return restCount
}

func countValidConfigurations(configuration []int, damageReport []int, memoizedResults *map[string]int) int {
	// fmt.Println("countValidConfigurations", formatConfiguration(configuration), damageReport)
	// can do this recursively
	// assumption: damageReport is all positive integers, no zeroes.
	// assumption: configuration has no "unknowns" in it.
	// -- can we weaken this?  let's try.

	key := fmt.Sprintf("%s|%s", formatConfiguration(configuration), formatDamageReport(damageReport))
	if (*memoizedResults)[key] != 0 {
		if (*memoizedResults)[key] == -1 {
			return 0
		}
		return (*memoizedResults)[key]
	}

	j := 0
	if len(damageReport) == 0 {
		// this implies the rest of the configuration is operational
		for _, k := range configuration {
			// unknowns are OK, if the damage report says nothing is left, they
			// have to be forced to OPERATIONAL.
			if k == DAMAGED {
				// fmt.Println("Invalid configuration empty report - returning 0")
				return storeMemoizedResults(memoizedResults, key, 0)
			}
		}
		return storeMemoizedResults(memoizedResults, key, 1)
	}

	i := damageReport[0]
	for j < len(configuration) && configuration[j] == OPERATIONAL {
		j++
	}

	// now we're at the start of a damaged (or unknown!) block.
	// we need it to be the size that we claimed it is.
	if i > len(configuration)-j {
		// Inconsistent
		return storeMemoizedResults(memoizedResults, key, 0)
	}

	didBlockStartWithUnknown := configuration[j] == UNKNOWN_STATE
	var restCount int
	if didBlockStartWithUnknown {
		// We could choose to not have this section in the damage report.
		restCount = countValidConfigurations(configuration[j+1:], damageReport, memoizedResults)
	}
	// now, we need it to be EXACTLY the size we want, and each entry needs to
	// be either damaged or unknown.
	for _, d := range configuration[j : j+i] {
		if d == OPERATIONAL {
			// Inconsistent
			return storeMemoizedResults(memoizedResults, key, restCount)
		}
	}

	// Now we're at the end of the block, and we need to either be at the end
	// of the string, or we need the damaged block to end, either through an
	// operational node or an unknown node.
	isAtEnd := j+i == len(configuration)
	if isAtEnd {
		restCount += countValidConfigurations(configuration[j+i:], damageReport[1:], memoizedResults)

		return storeMemoizedResults(memoizedResults, key, restCount)
	} else if configuration[j+i] != DAMAGED {
		// What we've seen so far is consistent, so check the rest.
		// One possibility is that this block started with an unknown, so
		// in that case, count without it as well.
		// We are assuming the damaged block STOPS here, so we actually need to
		// ignore the next entry.
		restCount += countValidConfigurations(configuration[j+i+1:], damageReport[1:], memoizedResults)
		return storeMemoizedResults(memoizedResults, key, restCount)
	} else {
		// Inconsistent with the start of the block being part of the damage
		// report, so assume it is operational and move forward.
		return storeMemoizedResults(memoizedResults, key, restCount)
	}
}

func day12(f *os.File) {
	scanner := bufio.NewScanner(f)
	total := 0
	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, " ")
		expandedConfig := ""
		expandedReport := ""
		for i := 0; i < 5; i++ {
			if i != 0 {
				expandedConfig += "?"
				expandedReport += ","
			}
			expandedConfig += res[0]
			expandedReport += res[1]
		}
		status := parseConfiguration(expandedConfig)
		damageReport := parseNumbers(strings.Split(expandedReport, ","))
		memoizedResults := make(map[string]int)
		validConfigs := countValidConfigurations(status, damageReport, &memoizedResults)
		total += validConfigs
	}
	fmt.Println(total)
}
