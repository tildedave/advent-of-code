package day4

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func isValidPassword(password string, partTwo bool) bool {
	if len(password) != 6 {
		return false
	}

	hasDuplicate := false
	hasDecrease := false
	for i := range password {
		if i == 0 {
			continue
		}
		if password[i] == password[i-1] && !partTwo {
			hasDuplicate = true
		}
		if password[i] < password[i-1] {
			hasDecrease = true
		}
	}
	if hasDecrease {
		return false
	}
	// 2 in a row is either a block of size 4 with ABBC or a starting block AAB
	// or an ending block ABB
	if partTwo {
		for j := range password {
			if j < len(password)-3 {
				section := password[j : j+4]
				if section[0] != section[1] && section[1] == section[2] && section[2] != section[3] {
					hasDuplicate = true
				}
			}
			if j == 0 {
				section := password[j : j+3]
				if section[0] == section[1] && section[1] != section[2] {
					hasDuplicate = true
				}
			}
			if j == len(password)-3 {
				section := password[j:]
				if section[0] != section[1] && section[1] == section[2] {
					hasDuplicate = true
				}
			}
		}
	}
	if !hasDuplicate {
		return false
	}
	return true
}

func Run(f *os.File, partTwo bool) {
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
		if isValidPassword(fmt.Sprintf("%d", i), partTwo) {
			validPasswordCount++
		}
	}
	fmt.Println(validPasswordCount)
}
