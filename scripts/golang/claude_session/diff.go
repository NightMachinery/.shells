package main

import (
	"fmt"
)

// ** diff

type diffOp struct {
	kind byte //: ' ', '-', '+'
	text string
}

// Line-based unified diff via an LCS table. Reports false when the inputs
// are big enough that the quadratic table is not worth it; the caller then
// falls back to printing both sides.
func unifiedDiff(a, b []string, ctx int) ([]string, bool) {
	n, m := len(a), len(b)
	if n*m > 4_000_000 {
		return nil, false
	}

	// dp[i*(m+1)+j] = length of the LCS of a[i:] and b[j:].
	dp := make([]uint32, (n+1)*(m+1))
	for i := n - 1; i >= 0; i-- {
		for j := m - 1; j >= 0; j-- {
			if a[i] == b[j] {
				dp[i*(m+1)+j] = dp[(i+1)*(m+1)+j+1] + 1
			} else if dp[(i+1)*(m+1)+j] >= dp[i*(m+1)+j+1] {
				dp[i*(m+1)+j] = dp[(i+1)*(m+1)+j]
			} else {
				dp[i*(m+1)+j] = dp[i*(m+1)+j+1]
			}
		}
	}

	var ops []diffOp
	i, j := 0, 0
	for i < n && j < m {
		switch {
		case a[i] == b[j]:
			ops = append(ops, diffOp{' ', a[i]})
			i, j = i+1, j+1
		case dp[(i+1)*(m+1)+j] >= dp[i*(m+1)+j+1]:
			ops = append(ops, diffOp{'-', a[i]})
			i++
		default:
			ops = append(ops, diffOp{'+', b[j]})
			j++
		}
	}
	for ; i < n; i++ {
		ops = append(ops, diffOp{'-', a[i]})
	}
	for ; j < m; j++ {
		ops = append(ops, diffOp{'+', b[j]})
	}

	return formatHunks(ops, ctx), true
}

func formatHunks(ops []diffOp, ctx int) []string {
	var out []string

	for start := 0; start < len(ops); {
		if ops[start].kind == ' ' {
			start++
			continue
		}

		// Grow the hunk while the next change is close enough that its
		// leading context would touch this one's trailing context, which is
		// where diff(1) merges them too.
		lo := start - ctx
		if lo < 0 {
			lo = 0
		}
		hi := start
		for hi < len(ops) {
			next := nextChange(ops, hi+1)
			if next >= 0 && next-hi-1 <= 2*ctx {
				hi = next
				continue
			}
			break
		}
		hi += ctx
		if hi > len(ops)-1 {
			hi = len(ops) - 1
		}

		aStart, bStart := 0, 0
		for _, op := range ops[:lo] {
			if op.kind != '+' {
				aStart++
			}
			if op.kind != '-' {
				bStart++
			}
		}

		aCount, bCount := 0, 0
		for _, op := range ops[lo : hi+1] {
			if op.kind != '+' {
				aCount++
			}
			if op.kind != '-' {
				bCount++
			}
		}

		out = append(out, fmt.Sprintf("@@ -%d,%d +%d,%d @@", aStart+1, aCount, bStart+1, bCount))
		for _, op := range ops[lo : hi+1] {
			out = append(out, string(op.kind)+op.text)
		}

		start = hi + 1
	}

	return out
}

func nextChange(ops []diffOp, from int) int {
	for i := from; i < len(ops); i++ {
		if ops[i].kind != ' ' {
			return i
		}
	}
	return -1
}
