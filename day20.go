package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strings"
	"unicode"
)

const TYPE_CONJUNCTION = 1
const TYPE_FLIP_FLOP = 2
const TYPE_BROADCASTER = 3
const STATE_LOW = 0
const STATE_HIGH = 1

type queueItem = struct {
	source string
	label  string
	pulse  bool
}

func summarizeStates(
	states []string,
	flipFlopStates map[string]bool,
	conjunctionStates map[string]map[string]bool,
	connections map[string][]string,
	types map[string]int) (string, int64) {
	ffRet := ""
	ffNum := int64(0)
	conjRet := ""
	for _, k := range states {
		if types[k] == TYPE_FLIP_FLOP {
			if flipFlopStates[k] {
				ffRet = ffRet + "1"
				ffNum = (ffNum << 1) | 1
			} else {
				ffRet = ffRet + "0"
				ffNum = (ffNum << 1) | 0
			}
		}
		if types[k] == TYPE_CONJUNCTION {
			conjRet += k + "{"
			for _, s := range connections[k] {
				if conjunctionStates[k][s] {
					conjRet += "1"
				} else {
					conjRet += "0"
				}
			}
			conjRet += "}"
		}
	}

	return ffRet + " " + conjRet, ffNum
}

// https://stackoverflow.com/a/67222540
type slice struct{ sort.StringSlice }

func (s slice) Less(d, e int) bool {
	t := strings.Map(unicode.ToUpper, s.StringSlice[d])
	u := strings.Map(unicode.ToUpper, s.StringSlice[e])
	return t < u
}

func day20(f *os.File) {
	scanner := bufio.NewScanner(f)
	connections := make(map[string][]string)
	types := make(map[string]int)

	flipFlopStates := make(map[string]bool)
	conjunctionStates := make(map[string]map[string]bool)
	states := make([]string, 0)

	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, "->")
		lhs := strings.TrimSpace(res[0])
		var lhsType int
		if lhs[0] == '%' {
			lhs = lhs[1:]
			lhsType = TYPE_FLIP_FLOP
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
		states = append(states, lhs)
	}

	a := slice{states}
	sort.Sort(a)
	states = a.StringSlice

	// set up the conjunction states
	for k, v := range connections {
		for _, b := range v {
			if types[b] == TYPE_CONJUNCTION {
				conjunctionStates[b][k] = false
			}
		}
	}

	top := ""
	bottom := ""
	for _, k := range states {
		if types[k] == TYPE_FLIP_FLOP {
			top += string(k[0])
			bottom += string(k[1])
		}
	}
	fmt.Println(top)
	fmt.Println(bottom)

	// fmt.Println("digraph {")
	// for _, k := range states {
	// 	if types[k] == TYPE_FLIP_FLOP {
	// 		fmt.Printf("%s [label=\"%%%s\"]\n", k, k)
	// 	} else if types[k] == TYPE_CONJUNCTION {
	// 		fmt.Printf("%s [label=\"&%s\"]\n", k, k)
	// 	}
	// 	for _, c := range connections[k] {
	// 		fmt.Printf("%s -> %s;\n", k, c)
	// 	}
	// }
	// fmt.Println("}")

	numHigh := 0
	numLow := 0
	numPushes := 0
	prevN := int64(-1)
	for numPushes < 1024 {
		s, n := summarizeStates(states, flipFlopStates, conjunctionStates, connections, types)
		fmt.Println(s, n^prevN)
		prevN = n
		numPushes++
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
			if item.label == "rx" && !item.pulse {
				fmt.Println(numPushes)
				return
			}
			switch types[item.label] {
			case TYPE_BROADCASTER:
				for _, c := range connections[item.label] {
					queue = append(queue, queueItem{item.label, c, item.pulse})
				}
			case TYPE_FLIP_FLOP:
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
	fmt.Println(numHigh * numLow)
}
