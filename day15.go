package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strconv"
	"strings"
)

func holidayASCIIStringHelper(s string) int {
	val := 0
	for i, _ := range s {
		val += int(s[i])
		val = val * 17
		val = val % 256
	}

	return val
}

type step = struct {
	label  string
	remove bool
	length int
}

func formatStep(s step) string {
	if s.remove {
		return fmt.Sprintf("%s-", s.label)
	}
	return fmt.Sprintf("%s=%d", s.label, s.length)
}

func day15(f *os.File) {
	scanner := bufio.NewScanner(f)
	boxes := make(map[int][]step)
	for scanner.Scan() {
		line := scanner.Text()
		fields := strings.Split(line, ",")
		for _, r := range fields {
			var s step
			if strings.Contains(r, "-") {
				// remove lens with this label from
				s = step{strings.Split(r, "-")[0], true, 0}
			} else {
				spl := strings.Split(r, "=")
				n, err := strconv.ParseInt(spl[1], 10, 64)
				if err != nil {
					log.Fatal(err)
				}
				s = step{spl[0], false, int(n)}
			}
			h := holidayASCIIStringHelper(s.label)
			l := boxes[h]
			if s.remove {
				for i, boxStep := range l {
					if boxStep.label == s.label {
						boxes[h] = append(l[0:i], l[i+1:]...)
						break
					}
				}
			} else {
				// add step
				didReplace := false
				for i, boxStep := range l {
					if boxStep.label == s.label {
						l[i] = s
						didReplace = true
						break
					}
				}
				if !didReplace {
					boxes[h] = append(boxes[h], s)
				}
			}
		}
	}
	focusingPower := 0
	for boxNum, l := range boxes {
		for slotNum, s := range l {
			focusingPower += (boxNum + 1) * (slotNum + 1) * s.length
		}
	}
	fmt.Println(focusingPower)
}
