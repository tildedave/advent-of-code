package intcode

import (
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

func Exec(program []int) []int {
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
			opModes = append(opModes, param_mode%10)
			param_mode /= 10
		}

		// TODO: need to remove repetition between add/mult

		switch opcode {
		case ADD_OPCODE:
			ops := make([]int, 0)
			for j := 0; j < 2; j++ {
				switch opModes[i] {
				case MODE_POSITION:
					ops = append(ops, result[result[i+1+j]])
				case MODE_IMMEDIATE:
					ops = append(ops, result[i+1+j])
				}
			}
			if opModes[2] == MODE_IMMEDIATE {
				panic("Invalid mode")
			}
			dest := result[i+3]
			result[dest] = ops[0] + ops[1]
		case MULT_OPCODE:
			ops := make([]int, 0)
			for j := 0; j < 2; j++ {
				switch opModes[i] {
				case MODE_POSITION:
					ops = append(ops, result[result[i+1+j]])
				case MODE_IMMEDIATE:
					ops = append(ops, result[i+1+j])
				}
			}
			if opModes[2] == MODE_IMMEDIATE {
				panic("Invalid mode")
			}
			dest := result[i+3]
			result[dest] = ops[0] * ops[1]
		case SAVE_OPCODE:
			// IDK
		case ENDING_OPCODE:
			return result
		default:
			log.Panicf("Invalid opcode: %d", opcode)
		}
		i += arity[opcode] + 1
	}
}
