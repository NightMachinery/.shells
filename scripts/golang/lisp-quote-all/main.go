package main

import (
	"bufio"
	"errors"
	"fmt"
	"io"
	"os"
)

func writeEscaped(bw *bufio.Writer, b []byte) error {
	start := 0
	for i, c := range b {
		if c == '"' || c == '\\' {
			if i > start {
				if _, err := bw.Write(b[start:i]); err != nil {
					return err
				}
			}
			if err := bw.WriteByte('\\'); err != nil {
				return err
			}
			if err := bw.WriteByte(c); err != nil {
				return err
			}
			start = i + 1
		}
	}
	if start < len(b) {
		if _, err := bw.Write(b[start:]); err != nil {
			return err
		}
	}
	return nil
}

func quoteAll(r io.Reader, w io.Writer) error {
	bw := bufio.NewWriterSize(w, 64*1024)
	if err := bw.WriteByte('"'); err != nil {
		return err
	}

	buf := make([]byte, 64*1024)
	for {
		n, err := r.Read(buf)
		if n > 0 {
			if werr := writeEscaped(bw, buf[:n]); werr != nil {
				return werr
			}
		}
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return err
		}
	}

	if _, err := bw.WriteString("\"\n"); err != nil {
		return err
	}
	return bw.Flush()
}

func main() {
	if err := quoteAll(os.Stdin, os.Stdout); err != nil {
		fmt.Fprintf(os.Stderr, "lisp-quote-all: %v\n", err)
		os.Exit(1)
	}
}
