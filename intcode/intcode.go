package intcode

import "log"

const ADD_OPCODE = 1
const MULT_OPCODE = 2
const ENDING_OPCODE = 99

func Exec(program []int) []int {
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
