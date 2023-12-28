package intcode

import (
	"errors"
	"log"
)

const ADD_OPCODE = 1
const MULT_OPCODE = 2
const INPUT_OPCODE = 3
const OUTPUT_OPCODE = 4
const JUMP_IF_TRUE_OPCODE = 5
const JUMP_IF_FALSE_OPCODE = 6
const LESS_THAN_OPCODE = 7
const EQUALS_OPCODE = 8
const RELATIVE_BASE_OFFSET_OPCODE = 9
const ENDING_OPCODE = 99

const MODE_POSITION = 0
const MODE_IMMEDIATE = 1
const MODE_RELATIVE = 2

var arity = map[int]int{
	ADD_OPCODE:                  3,
	MULT_OPCODE:                 3,
	INPUT_OPCODE:                1,
	OUTPUT_OPCODE:               1,
	JUMP_IF_TRUE_OPCODE:         2,
	JUMP_IF_FALSE_OPCODE:        2,
	LESS_THAN_OPCODE:            3,
	EQUALS_OPCODE:               3,
	RELATIVE_BASE_OFFSET_OPCODE: 1,
	ENDING_OPCODE:               0,
}

var args = map[int]int{
	ADD_OPCODE:                  2,
	MULT_OPCODE:                 2,
	INPUT_OPCODE:                1,
	OUTPUT_OPCODE:               1,
	LESS_THAN_OPCODE:            2,
	JUMP_IF_TRUE_OPCODE:         2,
	JUMP_IF_FALSE_OPCODE:        2,
	EQUALS_OPCODE:               2,
	RELATIVE_BASE_OFFSET_OPCODE: 1,
	ENDING_OPCODE:               0,
}

// Which opcodes store data
var hasDest = map[int]bool{
	ADD_OPCODE:       true,
	MULT_OPCODE:      true,
	INPUT_OPCODE:     true,
	LESS_THAN_OPCODE: true,
	EQUALS_OPCODE:    true,
}

func Exec(program []int) ([]int, error) {
	result, err := ExecFull(program, make(chan int), make(chan int))
	res := make([]int, len(result))
	for k, v := range result {
		res[k] = v
	}
	return res, err
}

func ExecFull(program []int, input chan int, output chan int) (map[int]int, error) {
	result := make(map[int]int)
	for n, i := range program {
		result[n] = i
	}
	numExecuted := 0
	i := 0
	currentRelativeBase := 0

	for {
		opcode := result[i] % 100
		param_mode := result[i] / 100
		// and then the parameter mode does other stuff.
		// DE - two-digit opcode,      02 == opcode 2
		//  C - mode of 1st parameter,  0 == position mode
		//  B - mode of 2nd parameter,  1 == immediate mode
		//  A - mode of 3rd parameter,  0 == position mode,
		//                                   omitted due to being a leading zero

		opModes := make([]int, arity[opcode])
		for j := 0; j < arity[opcode]; j++ {
			opModes[j] = param_mode % 10
			param_mode /= 10
		}

		ops := make([]int, args[opcode])
		for j := 0; j < args[opcode]; j++ {
			switch opModes[j] {
			case MODE_POSITION:
				ops[j] = result[result[i+1+j]]
			case MODE_IMMEDIATE:
				ops[j] = result[i+1+j]
			case MODE_RELATIVE:
				ops[j] = result[result[i+1+j]+currentRelativeBase]
			}
		}

		var dest int
		// for now assume the destination param is in location arity - 1
		if hasDest[opcode] {
			destParam := arity[opcode] - 1
			if opModes[destParam] == MODE_IMMEDIATE {
				return result, errors.New("specified MODE_IMMEDIATE for destination")
			}
			dest = result[i+1+destParam]
			if dest < 0 {
				return result, errors.New("attempted to access negative memory address")
			}
		}

		var increment bool = true

		switch opcode {
		case ADD_OPCODE:
			result[dest] = ops[0] + ops[1]
		case MULT_OPCODE:
			result[dest] = ops[0] * ops[1]
		case INPUT_OPCODE:
			value := <-input
			result[dest] = value
		case OUTPUT_OPCODE:
			output <- ops[0]
		case JUMP_IF_TRUE_OPCODE:
			value := ops[0]
			if value != 0 {
				i = ops[1]
				increment = false
			}
			// else nothing
		case JUMP_IF_FALSE_OPCODE:
			value := ops[0]
			if value == 0 {
				i = ops[1]
				increment = false
			}
			// else nothing
		case LESS_THAN_OPCODE:
			if ops[0] < ops[1] {
				result[dest] = 1
			} else {
				result[dest] = 0
			}
		case EQUALS_OPCODE:
			if ops[0] == ops[1] {
				result[dest] = 1
			} else {
				result[dest] = 0
			}
		case RELATIVE_BASE_OFFSET_OPCODE:
			currentRelativeBase += ops[0]
		case ENDING_OPCODE:
			close(output)
			return result, nil
		default:
			log.Panicf("Invalid opcode: %d", opcode)
		}
		if increment {
			i += arity[opcode] + 1
		}
		numExecuted++
	}
}
