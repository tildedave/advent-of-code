package day4

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func isValidPassword(password string, min int, max int) bool {
	if len(password) != 6 {
		return false
	}
	n, err := strconv.ParseInt(password, 10, 64)
	if err != nil {
		return false
	}
	if int(n) < min || int(n) > max {
		return false
	}

	hasDuplicate := false
	hasDecrease := false
	for i := range password {
		if i == 0 {
			continue
		}
		if password[i] == password[i-1] {
			hasDuplicate = true
		}
		if password[i] < password[i-1] {
			hasDecrease = true
		}
	}
	if !hasDuplicate {
		return false
	}
	if hasDecrease {
		return false
	}
	return true
}

func Run(f *os.File) {
	scanner := bufio.NewScanner(f)
	var max int64
	var min int64
	var err error
	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, "-")
		min, err = strconv.ParseInt(res[0], 10, 64)
		if err != nil {
			panic(err)
		}
		max, err = strconv.ParseInt(res[1], 10, 64)
		if err != nil {
			panic(err)
		}
	}
	validPasswordCount := 0
	for i := min; i <= max; i++ {
		if isValidPassword(fmt.Sprintf("%d", i), int(min), int(max)) {
			validPasswordCount++
		}
	}
	fmt.Println(validPasswordCount)
}
