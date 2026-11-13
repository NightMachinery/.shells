package main

import (
	"bufio"
	"bytes"
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

func quoteLines(r io.Reader, w io.Writer) error {
	br := bufio.NewReaderSize(r, 64*1024)
	bw := bufio.NewWriterSize(w, 64*1024)

	for {
		line, err := br.ReadBytes('\n')
		if len(line) > 0 {
			line = bytes.TrimSuffix(line, []byte{'\n'})
			if err := bw.WriteByte('"'); err != nil {
				return err
			}
			if err := writeEscaped(bw, line); err != nil {
				return err
			}
			if _, err := bw.WriteString("\"\n"); err != nil {
				return err
			}
		}
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
	}
	return bw.Flush()
}

func main() {
	if err := quoteLines(os.Stdin, os.Stdout); err != nil {
		fmt.Fprintf(os.Stderr, "lisp-quote: %v\n", err)
		os.Exit(1)
	}
}
