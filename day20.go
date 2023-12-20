package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

const TYPE_CONJUNCTION = 1
const TYPE_FLIP_FLIP = 2
const TYPE_BROADCASTER = 3
const STATE_LOW = 0
const STATE_HIGH = 1

type queueItem = struct {
	source string
	label  string
	pulse  bool
}

func day20(f *os.File) {
	scanner := bufio.NewScanner(f)
	connections := make(map[string][]string)
	types := make(map[string]int)

	flipFlopStates := make(map[string]bool)
	conjunctionStates := make(map[string]map[string]bool)

	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, "->")
		lhs := strings.TrimSpace(res[0])
		var lhsType int
		if lhs[0] == '%' {
			lhs = lhs[1:]
			lhsType = TYPE_FLIP_FLIP
		} else if lhs[0] == '&' {
			lhs = lhs[1:]
			lhsType = TYPE_CONJUNCTION
			conjunctionStates[lhs] = make(map[string]bool)
		} else if lhs == "broadcaster" {
			lhsType = TYPE_BROADCASTER
		} else {
			// type is implicit from the label
			lhsType = 0
		}
		connects := make([]string, 0)
		for _, rhs := range strings.Split(strings.TrimSpace(res[1]), ",") {
			connects = append(connects, strings.TrimSpace(rhs))
		}

		connections[lhs] = connects
		types[lhs] = lhsType
	}

	// set up the conjunction states
	for k, v := range connections {
		for _, b := range v {
			if types[b] == TYPE_CONJUNCTION {
				conjunctionStates[b][k] = false
			}
		}
	}

	numHigh := 0
	numLow := 0
	numPushes := 1_000
	for i := 0; i < numPushes; i++ {
		queue := make([]queueItem, 0)
		queue = append(queue, queueItem{"button", "broadcaster", false})
		for len(queue) > 0 {
			item := queue[0]
			// var pulseLabel string
			// if item.pulse {
			// 	pulseLabel = "high"
			// } else {
			// 	pulseLabel = "low"
			// }
			// fmt.Printf("%s -%s-> %s\n", item.source, pulseLabel, item.label)
			queue = queue[1:]
			if item.pulse {
				numHigh++
			} else {
				numLow++
			}
			switch types[item.label] {
			case TYPE_BROADCASTER:
				for _, c := range connections[item.label] {
					queue = append(queue, queueItem{item.label, c, item.pulse})
				}
			case TYPE_FLIP_FLIP:
				if !item.pulse {
					newState := !flipFlopStates[item.label]
					flipFlopStates[item.label] = newState
					for _, c := range connections[item.label] {
						queue = append(queue, queueItem{item.label, c, newState})
					}
				}
			case TYPE_CONJUNCTION:
				conjunctionStates[item.label][item.source] = item.pulse
				// sends low if all inputs have memory of high, sends high
				// otherwise.
				newState := true
				for _, v := range conjunctionStates[item.label] {
					newState = newState && v
				}

				for _, c := range connections[item.label] {
					queue = append(queue, queueItem{item.label, c, !newState})
				}
			}
		}
	}
	fmt.Println("high", numHigh, "low", numLow, numHigh*numLow)
}
