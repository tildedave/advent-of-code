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

func summarizeState(
	state *MachineState,
) (string, int64) {
	ffRet := ""
	ffNum := int64(0)
	conjRet := ""
	for _, k := range state.labels {
		if state.types[k] == TYPE_FLIP_FLOP {
			if state.flipFlopStates[k] {
				ffRet = ffRet + "1"
				ffNum = (ffNum << 1) | 1
			} else {
				ffRet = ffRet + "0"
				ffNum = (ffNum << 1) | 0
			}
		}
		if state.types[k] == TYPE_CONJUNCTION {
			conjRet += k + "{"
			for _, s := range state.connections[k] {
				if state.conjunctionStates[k][s] {
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

func toDOTString(
	state *MachineState,
) string {
	ret := ""
	ret += "digraph {"
	for _, k := range state.labels {
		if state.types[k] == TYPE_FLIP_FLOP {
			ret += fmt.Sprintf("%s [label=\"%%%s\"]\n", k, k)
		} else if state.types[k] == TYPE_CONJUNCTION {
			ret += fmt.Sprintf("%s [label=\"&%s\"]\n", k, k)
		}
		for _, c := range state.connections[k] {
			ret += fmt.Sprintf("%s -> %s;\n", k, c)
		}
	}
	ret += fmt.Sprintf("}")
	return ret
}

type MachineState = struct {
	labels            []string
	connections       map[string][]string
	types             map[string]int
	flipFlopStates    map[string]bool
	conjunctionStates map[string]map[string]bool
}

func pushButton(
	state *MachineState,
) (int, int, bool) {
	numHigh := 0
	numLow := 0

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
			return numHigh, numLow, true
		}
		switch state.types[item.label] {
		case TYPE_BROADCASTER:
			for _, c := range state.connections[item.label] {
				queue = append(queue, queueItem{item.label, c, item.pulse})
			}
		case TYPE_FLIP_FLOP:
			if !item.pulse {
				newState := !state.flipFlopStates[item.label]
				state.flipFlopStates[item.label] = newState
				for _, c := range state.connections[item.label] {
					queue = append(queue, queueItem{item.label, c, newState})
				}
			}
		case TYPE_CONJUNCTION:
			state.conjunctionStates[item.label][item.source] = item.pulse
			// sends low if all inputs have memory of high, sends high
			// otherwise.
			newState := true
			for _, v := range state.conjunctionStates[item.label] {
				newState = newState && v
			}

			for _, c := range state.connections[item.label] {
				queue = append(queue, queueItem{item.label, c, !newState})
			}
		}
	}
	return numHigh, numLow, false
}

func day20(f *os.File) {
	scanner := bufio.NewScanner(f)
	state := MachineState{}
	state.connections = make(map[string][]string)
	state.types = make(map[string]int)
	state.flipFlopStates = make(map[string]bool)
	state.conjunctionStates = make(map[string]map[string]bool)
	state.labels = make([]string, 0)

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
			state.conjunctionStates[lhs] = make(map[string]bool)
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

		state.connections[lhs] = connects
		state.types[lhs] = lhsType
		state.labels = append(state.labels, lhs)
	}

	a := slice{state.labels}
	sort.Sort(a)
	state.labels = a.StringSlice

	// set up the conjunction states
	for k, v := range state.connections {
		for _, b := range v {
			if state.types[b] == TYPE_CONJUNCTION {
				state.conjunctionStates[b][k] = false
			}
		}
	}

	top := ""
	bottom := ""
	for _, k := range state.labels {
		if state.types[k] == TYPE_FLIP_FLOP {
			top += string(k[0])
			bottom += string(k[1])
		}
	}
	fmt.Println(top)
	fmt.Println(bottom)

	numHigh := 0
	numLow := 0
	numPushes := 0
	prevN := int64(-1)
	for numPushes < 1000 {
		s, n := summarizeState(&state)
		fmt.Println(s, n^prevN)
		prevN = n
		numPushes++
		high, low, _ := pushButton(&state)
		numHigh += high
		numLow += low
	}
	fmt.Println(numHigh * numLow)
}
