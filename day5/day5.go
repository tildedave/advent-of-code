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
	output := make(chan int)
	halt := make(chan bool)
	go intcode.ExecFull(program, input, output, halt)
	input <- 1
	for o := range output {
		fmt.Println(o)
	}
	h := <-halt
	fmt.Println(h)
}
