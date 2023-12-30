package day12

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"

	"github.com/tildedave/advent-of-code-2019/utils"
)

func ComputeVelocities(positions [][]int, velocities [][]int) [][]int {
	for i := range positions {
		if velocities[i] == nil {
			velocities[i] = make([]int, 3)
		}

		for j := range positions {
			if velocities[j] == nil {
				velocities[j] = make([]int, 3)
			}

			if !(i < j) {
				continue
			}
			for n := 0; n < 3; n++ {
				if positions[i][n] < positions[j][n] {
					velocities[i][n]++
					velocities[j][n]--
				} else if positions[i][n] > positions[j][n] {
					velocities[i][n]--
					velocities[j][n]++
				}
			}
		}
	}
	return velocities
}

func TotalEnergy(positions [][]int, velocities [][]int) int {
	total := 0
	for i := range positions {
		position := positions[i]
		velocity := velocities[i]
		potential := 0
		for _, p := range position {
			potential += utils.AbsInt(p)
		}
		kinetic := 0
		for _, v := range velocity {
			kinetic += utils.AbsInt(v)
		}
		total += potential * kinetic
	}

	return total
}

func Run(f *os.File, partTwo bool) {
	c := regexp.MustCompile(`^<x=(-?\d+)+, y=(-?\d+), z=(-?\d+)>$`)
	scanner := bufio.NewScanner(f)
	positions := make([][]int, 0)

	for scanner.Scan() {
		line := scanner.Text()

		pos, err := utils.ParseNumberList(c.FindStringSubmatch(line)[1:])
		if err != nil {
			log.Fatal(err)
		}
		positions = append(positions, pos)
	}

	velocities := make([][]int, len(positions))
	for i := range positions {
		velocities[i] = make([]int, 3)
	}

	numSteps := 1000
	for x := 0; x <= numSteps; x++ {
		for i := range positions {
			for n := 0; n < 3; n++ {
				positions[i][n] += velocities[i][n]
			}
		}
		if x != numSteps {
			velocities = ComputeVelocities(positions, velocities)
		}
	}
	fmt.Println(TotalEnergy(positions, velocities))
}
