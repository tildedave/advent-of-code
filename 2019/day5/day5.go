package day5

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
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

	input := make(chan int)
	output := make(chan int, 100)
	go intcode.ExecFull(program, input, output)

	if !partTwo {
		input <- 1
		for {
			o, ok := <-output
			if !ok {
				break
			}
			fmt.Println(o)
		}
	} else {
		input <- 5
		o := <-output
		fmt.Println(o)
	}
}
