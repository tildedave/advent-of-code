package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

type rule = struct {
	category    string
	operation   string
	conditional int
	dest        string
}
type workflow = struct {
	rules []rule
	dest  string
}

type queueEntry = struct {
	criteria map[string][]int
	label    string
}

func makeCriteria(x []int, m []int, a []int, s []int) map[string][]int {
	criteria := make(map[string][]int)
	criteria["x"] = []int{1, 4000}
	criteria["m"] = []int{1, 4000}
	criteria["a"] = []int{1, 4000}
	criteria["s"] = []int{1, 4000}

	return criteria
}

func copyCriteria(criteria map[string][]int) map[string][]int {
	ret := make(map[string][]int)
	for k, v := range criteria {
		ret[k] = make([]int, len(v))
		copy(ret[k], criteria[k])
	}
	return ret
}

func day19(f *os.File) {
	scanner := bufio.NewScanner(f)
	workflows := make(map[string]workflow)
	parts := make([]map[string]int, 0)

	for scanner.Scan() {
		line := scanner.Text()
		if len(line) == 0 {
			continue
		}
		if line[0] == '{' {
			res := strings.Split(line[1:len(line)-1], ",")
			part := make(map[string]int)
			parts = append(parts, part)
			for _, p := range res {
				r := strings.Split(p, "=")
				n, err := strconv.ParseInt(r[1], 10, 64)
				if err != nil {
					log.Fatal(err)
				}
				part[r[0]] = int(n)
			}
		} else {
			// workflow
			wstr := strings.Split(strings.ReplaceAll(line, "}", ""), "{")
			workflowLabel := wstr[0]
			w := workflow{}
			rules := make([]rule, 0)

			for _, r := range strings.Split(wstr[1], ",") {
				if !strings.Contains(r, ":") {
					w.dest = r
					continue
				}
				currRule := rule{}
				rSplit := strings.Split(r, ":")
				currRule.dest = rSplit[1]
				if strings.Contains(rSplit[0], ">") {
					arr := strings.Split(rSplit[0], ">")
					currRule.operation = ">"
					currRule.category = arr[0]
					n, err := strconv.ParseInt(arr[1], 10, 64)
					if err != nil {
						log.Fatal(err)
					}
					currRule.conditional = int(n)
				} else if strings.Contains(rSplit[0], "<") {
					arr := strings.Split(rSplit[0], "<")
					currRule.operation = "<"
					currRule.category = arr[0]
					n, err := strconv.ParseInt(arr[1], 10, 64)
					if err != nil {
						log.Fatal(err)
					}
					currRule.conditional = int(n)
				} else {
					panic("Invalid input")
				}
				rules = append(rules, currRule)
			}
			w.rules = rules
			workflows[workflowLabel] = w
		}
	}

	criteria := makeCriteria([]int{1, 4000}, []int{1, 4000}, []int{1, 4000}, []int{1, 4000})
	queue := make([]queueEntry, 0)
	queue = append(queue, queueEntry{criteria, "in"})
	accepted := make([]map[string][]int, 0)

	for len(queue) > 0 {
		item := queue[0]
		queue = queue[1:]
		currentCriteria := copyCriteria(item.criteria)

		if item.label == "R" {
			continue
		}

		if item.label == "A" {
			accepted = append(accepted, currentCriteria)
			continue
		}

		isConsistent := true
		for _, v := range currentCriteria {
			if !(v[0] < v[1]) {
				fmt.Println("currentCriteria inconsistent, continuing")
				isConsistent = false
				continue
			}
		}
		if !isConsistent {
			continue
		}

		w := workflows[item.label]
		for _, r := range w.rules {
			// add a new condition to the queue
			nextCriteria := copyCriteria(currentCriteria)

			if r.operation == "<" {
				currentCriteria[r.category][0] = r.conditional
				nextCriteria[r.category][1] = r.conditional - 1
			} else if r.operation == ">" {
				currentCriteria[r.category][1] = r.conditional
				nextCriteria[r.category][0] = r.conditional + 1
			}

			nextItem := queueEntry{nextCriteria, r.dest}
			queue = append(queue, nextItem)
		}
		nextItem := queueEntry{currentCriteria, w.dest}
		queue = append(queue, nextItem)
	}

	total := 0
	for _, a := range accepted {
		possibilities := 1
		for _, v := range a {
			possibilities *= (v[1] - v[0] + 1)
		}
		total += possibilities
	}
	fmt.Println(total)
}
