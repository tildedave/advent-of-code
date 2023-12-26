package intcode

import (
	"errors"
	"log"
)

const ADD_OPCODE = 1
const MULT_OPCODE = 2
const SAVE_OPCODE = 3
const ENDING_OPCODE = 99

const MODE_POSITION = 0
const MODE_IMMEDIATE = 1

var arity = map[int]int{
	ADD_OPCODE:    3,
	MULT_OPCODE:   3,
	SAVE_OPCODE:   0,
	ENDING_OPCODE: 0,
}

var args = map[int]int{
	ADD_OPCODE:    2,
	MULT_OPCODE:   2,
	SAVE_OPCODE:   0,
	ENDING_OPCODE: 0,
}

var dest = map[int]int{
	ADD_OPCODE:    2,
	MULT_OPCODE:   2,
	SAVE_OPCODE:   0,
	ENDING_OPCODE: 0,
}

func Exec(program []int) ([]int, error) {
	result := make([]int, len(program))
	copy(result, program)
	i := 0

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
			}
		}

		destParam := dest[opcode]
		if destParam > 0 && opModes[destParam] == MODE_IMMEDIATE {
			return result, errors.New("specified MODE_IMMEDIATE for destination")
		}

		switch opcode {
		case ADD_OPCODE:
			dest := result[i+3]
			result[dest] = ops[0] + ops[1]
		case MULT_OPCODE:
			dest := result[i+3]
			result[dest] = ops[0] * ops[1]
		case SAVE_OPCODE:
			// IDK
		case ENDING_OPCODE:
			return result, nil
		default:
			log.Panicf("Invalid opcode: %d", opcode)
		}
		i += arity[opcode] + 1
	}
}
