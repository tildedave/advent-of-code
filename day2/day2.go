package day2

import (
	"bufio"
	"fmt"
	"log"
	"math/rand"
	"os"
	"strings"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File) {
	scanner := bufio.NewScanner(f)
	var program []int
	rows := 0
	for scanner.Scan() {
		if rows > 0 {
			panic("Only should have had one line of input")
		}
		line := scanner.Text()
		p, err := utils.ParseNumberList(strings.Split(line, ","))
		if err != nil {
			log.Fatal(err)
		}
		program = p
		rows++
	}

	for {
		i := rand.Intn(100)
		j := rand.Intn(100)
		// now we run the program
		program[1] = i
		program[2] = j
		result, _ := intcode.Exec(program)
		if result[0] == 19690720 {
			fmt.Println(i, j)
			return
		}
	}
}
