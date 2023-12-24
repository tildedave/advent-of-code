package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
)

type vector = struct {
	p [3]float64
	v [3]float64
}

func sub2d(v [3]float64, w [3]float64) [3]float64 {
	return [3]float64{v[0] - w[0], v[1] - w[1]}
}
func add2d(v [3]float64, w [3]float64) [3]float64 {
	return [3]float64{v[0] + w[0], v[1] + w[1]}
}

func scalar_mult(v [3]float64, c float64) [3]float64 {
	return [3]float64{c * float64(v[0]), c * float64(v[1])}
}

func cross2d(v [3]float64, w [3]float64) float64 {
	// 2d cross product
	// v x w = vx wy - vy wx
	return v[0]*w[1] - v[1]*w[0]
}

func day24(f *os.File) {
	scanner := bufio.NewScanner(f)
	vecs := make([]vector, 0)
	for scanner.Scan() {
		line := scanner.Text()
		res := strings.Split(line, " @ ")
		p := parseNumbers(strings.Split(res[0], ", "))
		v := parseNumbers(strings.Split(res[1], ", "))

		pf := make([]float64, 0)
		for _, n := range p {
			pf = append(pf, float64(n))
		}
		vf := make([]float64, 0)
		for _, n := range v {
			vf = append(vf, float64(n))
		}

		vecs = append(vecs, vector{[3]float64(pf), [3]float64(vf)})
	}

	// for each pair of vectors, see if it intersects.
	// we're also ignoring the z component for the first part here.
	// p1 + v1t = p2 + v2t
	// t = (v1 - v2)^{-1} * (p1 - p2)
	// essentially all vectors will intersect assuming they are not parallel
	// (parallel postulate)

	numIntersect := 0
	coordRange := [2]float64{200000000000000, 400000000000000}
	for i, v1 := range vecs {
		for j, v2 := range vecs {
			if !(i < j) {
				continue
			}
			point_diff := sub2d(v2.p, v1.p)
			cross_result := cross2d(v1.v, v2.v)
			t := cross2d(point_diff, v2.v) / cross_result
			u := cross2d(point_diff, v1.v) / cross_result

			// otherwise see if it's in the range that we want

			intersect1 := add2d(v1.p, scalar_mult(v1.v, t))
			intersect2 := add2d(v2.p, scalar_mult(v2.v, u))
			fmt.Println(intersect1, intersect2)

			if t < 0 || u < 0 {
				continue
			}

			if intersect1[0] >= coordRange[0] && intersect1[0] <= coordRange[1] && intersect1[1] >= coordRange[0] && intersect1[1] <= coordRange[1] {
				numIntersect++
			}
		}
	}
	fmt.Println(numIntersect)
}
