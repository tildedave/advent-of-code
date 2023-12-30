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

func hashCoordinates(positions [][]int, velocities [][]int, n int) string {
	str := ""
	for i, p := range positions {
		if i != 0 {
			str += "|"
		}
		str += fmt.Sprintf("%d", p[n])
	}
	for _, v := range velocities {
		str += "|"
		str += fmt.Sprintf("%d", v[n])
	}
	return str
}

func Run(f *os.File, partTwo bool) {
	c := regexp.MustCompile(`^<x=(-?\d+)+, y=(-?\d+), z=(-?\d+)>$`)
	scanner := bufio.NewScanner(f)
	initialPositions := make([][]int, 0)

	for scanner.Scan() {
		line := scanner.Text()

		pos, err := utils.ParseNumberList(c.FindStringSubmatch(line)[1:])
		if err != nil {
			log.Fatal(err)
		}
		initialPositions = append(initialPositions, pos)
	}

	positions := make([][]int, len(initialPositions))
	for i := range positions {
		positions[i] = make([]int, 3)
		copy(positions[i], initialPositions[i])
	}

	velocities := make([][]int, len(positions))
	for i := range positions {
		velocities[i] = make([]int, 3)
	}

	if !partTwo {
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
		return
	}

	cycles := make([]int, 0)
	for n := 0; n < 3; n++ {
		positions = initialPositions
		velocities := make([][]int, len(positions))
		for i := range positions {
			velocities[i] = make([]int, 3)
		}
		initial := hashCoordinates(positions, velocities, n)
		num := 0
		for {
			for i := range positions {
				positions[i][n] += velocities[i][n]
			}
			if num > 0 && hashCoordinates(positions, velocities, n) == initial {
				cycles = append(cycles, num)
				break
			}
			// we actually only have to compute the velocity for position n
			// but it's more convenient (I don't want to add arguments) to do
			// for each of them
			velocities = ComputeVelocities(positions, velocities)
			num++
		}
	}
	// answer is LCM of cycles - computed via Sage
	fmt.Println(cycles)
}
