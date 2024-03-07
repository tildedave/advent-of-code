package day25

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
	"sync"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	reader := bufio.NewReader(os.Stdin)
	program := utils.ParseProgram(f)
	input := make(chan int)
	output := make(chan int)
	var wg sync.WaitGroup
	wg.Add(1)
	go func() {
		intcode.ExecFull(program, input, output)
		defer wg.Done()
	}()

	go func() {
		runes := make([]rune, 0)
		for o := range output {
			if o == 10 {
				line := string(runes)
				fmt.Println(line)
				if line == "Command?" {
					text, err := reader.ReadString('\n')
					if err != nil {
						log.Fatalf("Error reading from input: %s", err)
					}
					switch strings.TrimSpace(text) {
					case "n":
						text = "north\n"
					case "s":
						text = "south\n"
					case "e":
						text = "east\n"
					case "w":
						text = "west\n"
					}
					for _, o := range text {
						input <- int(o)
					}
				}
				runes = make([]rune, 0)
			} else {
				runes = append(runes, rune(o))
			}
		}
	}()

	wg.Wait()
}
