package day21

import (
	"fmt"
	"log"
	"os"
	"strings"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	var wg sync.WaitGroup
	input := make(chan int)
	output := make(chan int)
	wg.Add(1)
	go func() {
		intcode.ExecFull(program, input, output)
		defer wg.Done()
	}()

	for ch := range output {
		fmt.Print(string(rune(ch)))
		if ch == 10 {
			break
		}
	}

	// Jumping clears 3 tiles.  We want to jump if we would die in the next
	// two, and if 4 tiles away is safe.

	// Jump if: (A FALSE OR b FALSE OR c FALSE) AND D true.

	// We also need to clear the JUMP register.
	// I wonder if we want to carry the T + J forward.
	// How to program: if jump is true, set to false.  Otherwise set to false.

	// Clear jump register
	// NOT J T
	// AND T J
	// Jump if A or B or C is false.
	// NOT A T
	// OR T J
	// NOT B T
	// OR T J
	// NOT C T
	// OR T J
	// However, don't jump if D is false.
	// AND D J

	// So we need (A true OR B true) AND C false.

	var instructions []string
	if !partTwo {
		// J = (~A V ~B ^ ~C) ^ D
		// Demorgan gives:
		// J = ~(A ^ B ^ C) ^ D
		instructions = []string{
			// clear J
			"NOT J T",
			"AND T J",
			// A B C
			"AND A T",
			"AND B T",
			"AND C T",
			"NOT T J",
			"AND D J",
			"WALK",
		}
	} else {
		// Situation that the first program does not work with.
		// .................
		// .................
		// @................
		// #####.#.#...#####
		// We still jump the same amount as in part 1.
		// We want much of the same logic.
		// We still need a "danger sensor" and a sanity checker.
		// Here we end up on the first "#" but there's no way to jump to
		// safety.  We should have jumped earlier basically.
		// It's not obvious to me yet how E F G H I will be needed.
		// OK, we DON'T want to jump to a region if then that region is
		// dangerous.

		// so this is similar to the previous:
		// Jump if: (~A v ~B v ~C) ^ D ^ ~(~E ^ ~H)
		// ~(A ^ B ^ C) ^ D ^ (E v H)

		// BUT, don't jump if:
		// (~E) ^ (~H)
		// == ~(E v H)

		instructions = []string{
			// clear jump register
			"NOT J T",
			"AND T J", // J is now false and T is true
			// We must jump immediately
			"AND A T",
			"AND B T",
			"AND C T",
			"NOT T J",
			"AND D J",
			"OR E T",
			"OR H T",
			"AND T J",
			"RUN",
		}
	}
	if len(instructions) > 15 {
		log.Fatalf("Too long: %d", len(instructions))
	}
	for _, ch := range strings.Join(instructions, "\n") {
		input <- int(ch)
	}
	input <- 10
	for ch := range output {
		if ch >= 0 && ch <= 127 {
			fmt.Print(string(rune(ch)))
		} else {
			// our answer
			fmt.Print(ch)
		}
	}
	wg.Wait()
}
