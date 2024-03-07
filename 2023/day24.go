package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"

	"gonum.org/v1/gonum/mat"
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

func sub3d(v [3]float64, w [3]float64) [3]float64 {
	return [3]float64{v[0] - w[0], v[1] - w[1], v[2] - w[2]}
}

func add3d(v [3]float64, w [3]float64) [3]float64 {
	return [3]float64{v[0] + w[0], v[1] + w[1], v[2] + w[2]}
}

func scalar_mult(v [3]float64, c float64) [3]float64 {
	return [3]float64{c * float64(v[0]), c * float64(v[1]), c * float64(v[2])}
}

func dot3d(v [3]float64, w [3]float64) float64 {
	return v[0]*w[0] + v[1]*w[1] + v[2]*w[2]
}

func cross2d(v [3]float64, w [3]float64) float64 {
	// 2d cross product
	// v x w = vx wy - vy wx
	return v[0]*w[1] - v[1]*w[0]
}

func cross3d(v [3]float64, w [3]float64) [3]float64 {
	return [3]float64{
		v[1]*w[2] - v[2]*w[1],
		v[2]*w[0] - v[0]*w[2],
		v[0]*w[1] - v[1]*w[0],
	}
}

func intersect2d(v vector, w vector) bool {
	point_diff := sub2d(w.p, v.p)
	cross_result := cross2d(v.v, w.v)
	t := cross2d(point_diff, w.v) / cross_result
	u := cross2d(point_diff, v.v) / cross_result

	// otherwise see if it's in the range that we want
	// we actually don't need these for part 2.
	// intersect1 := add2d(v.p, scalar_mult(v.v, t))
	// intersect2 := add2d(w.p, scalar_mult(w.v, u))

	if t < 0 || u < 0 {
		return false
	}
	return true
}

func intersect3d(v vector, w vector, chatty bool) bool {
	a := mat.NewDense(3, 2, []float64{
		-v.v[0], w.v[0],
		-v.v[1], w.v[1],
		-v.v[2], w.v[2],
	})
	b := mat.NewDense(3, 1, []float64{
		v.p[0] - w.p[0],
		v.p[1] - w.p[1],
		v.p[2] - w.p[2],
	})
	var x mat.Dense
	err := x.Solve(a, b)

	if err != nil {
		return false
		// log.Fatalf("no solution: %v", err)
	}
	ret := x.At(0, 0) > 0 && x.At(1, 0) > 0
	if chatty {
		fmt.Println(add3d(v.p, scalar_mult(v.v, x.At(0, 0))))
		fmt.Println(add3d(w.p, scalar_mult(w.v, x.At(1, 0))))
		q := add3d(v.p, scalar_mult(v.v, x.At(0, 0)))
		fmt.Println(int(q[0]) + int(q[1]) + int(q[2]))
	}
	return ret
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

	// a := mat.NewDense(3, 2, []float64{
	// 	-1, 2,
	// 	0, -2,
	// 	4, -4,
	// })
	// b := mat.NewDense(3, 1, []float64{
	// 	1,
	// 	-6,
	// 	8,
	// })
	// fmt.Println(a, b)
	// var x mat.Dense
	// err := x.Solve(a, b)
	// if err != nil {
	// 	log.Fatalf("no solution: %v", err)
	// }
	// fmt.Println("Solve", x)

	// return

	r := 500

	// brute force idea from reddit
	for vx := -r; vx <= r; vx++ {
		for vy := -r; vy <= r; vy++ {
			velocity_adj := [3]float64{float64(vx), float64(vy), 0}
			hasBad := false
		Check2d:
			for i, v := range vecs {
				for j, w := range vecs {
					if !(i < j) {
						continue
					}

					v_adj := sub3d(v.v, velocity_adj)
					w_adj := sub3d(w.v, velocity_adj)

					if !intersect2d(vector{v.p, v_adj}, vector{w.p, w_adj}) {
						hasBad = true
						break Check2d
					}
				}
			}
			if hasBad {
				continue
			}
			fmt.Println(velocity_adj, "might be good")

			for vz := 0; vz <= r; vz++ {
				velocity_adj[2] = float64(vz)
				// fmt.Println("we test", velocity_adj)

				hasBad = false

			CheckVelocity:
				for i, v := range vecs {
					for j, w := range vecs {
						if !(i < j) {
							continue
						}

						v_adj := sub3d(v.v, velocity_adj)
						w_adj := sub3d(w.v, velocity_adj)

						// fmt.Println("testing", vz, v_adj, w_adj)
						if !intersect3d(vector{v.p, v_adj}, vector{w.p, w_adj}, false) {
							hasBad = true
							break CheckVelocity
						}
					}
				}

				if !hasBad {
					fmt.Println(vx, vy, vz, "looks good")

					for i, v := range vecs {
						for j, w := range vecs {
							if !(i < j) {
								continue
							}

							v_adj := sub3d(v.v, velocity_adj)
							w_adj := sub3d(w.v, velocity_adj)

							// fmt.Println("testing", vz, v_adj, w_adj)
							intersect3d(vector{v.p, v_adj}, vector{w.p, w_adj}, true)
						}
					}
				}
			}
		}
	}
}
