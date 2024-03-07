package utils

import (
	"bufio"
	"log"
	"os"
	"strconv"
	"strings"
)

func ParseNumberList(strlist []string) ([]int, error) {
	ret := make([]int, 0)
	for _, nStr := range strlist {
		n, err := strconv.ParseInt(nStr, 10, 64)
		if err != nil {
			return []int{}, err
		}
		ret = append(ret, int(n))
	}

	return ret, nil
}

func ParseProgram(f *os.File) []int {
	scanner := bufio.NewScanner(f)
	var program []int
	rows := 0
	for scanner.Scan() {
		if rows > 0 {
			panic("Only should have had one line of input")
		}
		line := scanner.Text()
		p, err := ParseNumberList(strings.Split(line, ","))
		if err != nil {
			log.Fatal(err)
		}
		program = p
		rows++
	}
	return program
}

func ReadSingleLine(f *os.File) string {
	var line string
	rows := 0

	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		if rows > 0 {
			panic("Can only process one row")
		}
		line = scanner.Text()
		rows++
	}
	return line
}

// https://stackoverflow.com/a/30230552
func NextPermutation(p []int) {
	for i := len(p) - 1; i >= 0; i-- {
		if i == 0 || p[i] < len(p)-i-1 {
			p[i]++
			return
		}
		p[i] = 0
	}
}

func GetPermutation(original, p []int) []int {
	result := make([]int, len(original))
	copy(result, original)

	for i, v := range p {
		result[i], result[i+v] = result[i+v], result[i]
	}
	return result
}

func AbsInt(i int) int {
	if i < 0 {
		return -i
	}
	return i
}

func IsLowercase(c byte) bool {
	i := int(c)
	return i >= 97 && i <= 122
}

func IsUppercase(c byte) bool {
	i := int(c)
	return i >= 65 && i <= 90
}

func MaxInt(i1, i2 int) int {
	if i1 < i2 {
		return i2
	}
	return i1
}

func ModPositive[T int | int64](n T, m T) T {
	r := n % m
	if r < 0 {
		return m + r
	}
	return r
}

func ModMult[T int | int64](a T, b T, p T) T {
	// long double x;
	// uint64_t c;
	// int64_t r;
	// if (a >= m) a %= m;
	// if (b >= m) b %= m;

	var res T = 0
	for b > 0 {
		if b%2 == 1 {
			res = ModPositive(res+a, p)
		}
		b >>= 1
		if b != 0 {
			a = (a + a) % p
		}
	}
	return res
}

// Exp returns m^n % p using the method of repeated squaring
func ModExp[T int | int64](m T, n T, p T) T {
	var pow T = 1

	for n > 0 {
		if n%2 == 1 {
			pow = ModMult(pow, m, p)
		}
		m = ModMult(m, m, p)
		n = n >> 1
	}

	return pow
}

func ModInverse[T int | int64](x T, p T) T {
	// This method only works for primes, which the deck size conveniently is.
	// a^{p-1} = 1 mod p
	// a * a^{p-2} = 1 mod p
	if x < 0 {
		x += p
	}
	return ModExp(x, p-2, p)
}
