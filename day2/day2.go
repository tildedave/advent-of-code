package day2

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/tildedave/advent-of-code-2019/utils"
)

const ADD_OPCODE = 1
const MULT_OPCODE = 2
const ENDING_OPCODE = 99

func IntcodeExec(program []int) []int {
	result := make([]int, len(program))
	copy(result, program)
	i := 0

	for {
		switch result[i] {
		case ADD_OPCODE:
			op1 := result[result[i+1]]
			op2 := result[result[i+2]]
			dest := result[i+3]
			result[dest] = op1 + op2
			i += 4
		case MULT_OPCODE:
			op1 := result[result[i+1]]
			op2 := result[result[i+2]]
			dest := result[i+3]
			result[dest] = op1 * op2
			i += 4
		case ENDING_OPCODE:
			return result
		default:
			log.Panicf("Invalid opcode: %d", result[i])
		}
	}
}

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
	fmt.Println(program)

	// now we run the program
	program[1] = 12
	program[2] = 2
	result := IntcodeExec(program)
	fmt.Println(result[0])
}
