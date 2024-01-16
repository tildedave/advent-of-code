package day23

import (
	"fmt"
	"os"
	"time"

	"github.com/tildedave/advent-of-code-2019/intcode"
	"github.com/tildedave/advent-of-code-2019/utils"
)

func Run(f *os.File, partTwo bool) {
	program := utils.ParseProgram(f)
	numComputers := 50
	bufferSize := 20
	// we want to fan in outputs to a single channel where we can send
	// to the appropriate channels.
	fanInOutput := make(chan [4]int)

	inputChannels := make([]chan int, numComputers)
	outputChannels := make([]chan int, numComputers)
	for n := 0; n < numComputers; n++ {
		output := make(chan int, bufferSize)
		input := make(chan int, bufferSize)
		inputChannels[n] = input
		outputChannels[n] = output

		go func(n int) {
			for dest := range output {
				x := <-output
				y := <-output
				fanInOutput <- [4]int{n, dest, x, y}
			}
		}(n)

		go func(n int) {
			intcode.ExecFullWithDefault(program, input, output, -1)
			// do we need to wait for completion?
		}(n)

		input <- n
	}

	var lastNatPacket [2]int
	var lastSentPacket time.Time
	// var lastSentY int

	// This is the NAT
	if partTwo {
		go func() {
			tickChan := time.NewTicker(time.Millisecond * 100)
			for range tickChan.C {
				// check if network is idle
				if lastNatPacket == [2]int{} {
					continue
				}

				if time.Since(lastSentPacket) <= time.Second*5 {
					// fmt.Println(time.Since(lastSentPacket))
					continue
				}

				// if lastSentY == lastNatPacket[1] {
				// 	fmt.Println("[NAT] received duplicate NAT packet, we are done!")
				// 	fmt.Println(lastSentY)
				// 	break
				// }

				// otherwise send to address 0
				inputChannels[0] <- lastNatPacket[0]
				inputChannels[0] <- lastNatPacket[1]
				// lastSentY = lastNatPacket[1]
				fmt.Printf("[NAT] detected idle, sending (%d, %d) to %d\n", lastNatPacket[0], lastNatPacket[1], 0)
			}
		}()
	}

	for packet := range fanInOutput {
		lastSentPacket = time.Now()
		_, dest, x, y := packet[0], packet[1], packet[2], packet[3]
		if dest == 255 {
			if !partTwo {
				// done
				fmt.Println(y)
				break
			} else {
				lastNatPacket = [2]int{x, y}
				continue
			}
		}

		fmt.Printf("[%d] sending (%d, %d) to %d\n", packet[0], x, y, dest)
		inputChannels[dest] <- x
		inputChannels[dest] <- y
	}
}
