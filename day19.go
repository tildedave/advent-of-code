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

	total := 0
	for _, p := range parts {
		label := "in"
		for label != "A" && label != "R" {
			w := workflows[label]
			nextLabel := w.dest
			for _, r := range w.rules {
				operand := r.category
				if (r.operation == "<" && p[operand] < r.conditional) ||
					(r.operation == ">" && p[operand] > r.conditional) {
					nextLabel = r.dest
					break
				}
			}
			label = nextLabel
		}
		if label == "A" {
			for _, v := range p {
				total += v
			}
		}
	}
	fmt.Println(total)
}
