package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"sort"
	"strings"
	"sync"
)

const workerCount = 16

type fileCount struct {
	path  string
	lines int64
}

func parseFileList(r io.Reader) ([]string, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	if len(data) == 0 {
		return nil, nil
	}
	files := strings.Split(string(data), "\n")
	// Match cl-ppcre:split #\Newline default behavior, which drops trailing empty fields.
	for len(files) > 0 && files[len(files)-1] == "" {
		files = files[:len(files)-1]
	}
	return files, nil
}

func countFileNewlines(path string) (int64, error) {
	f, err := os.Open(path)
	if err != nil {
		return 0, err
	}
	defer f.Close()

	buf := make([]byte, 64*1024)
	var count int64
	newline := []byte{'\n'}
	for {
		n, err := f.Read(buf)
		if n > 0 {
			count += int64(bytes.Count(buf[:n], newline))
		}
		if errors.Is(err, io.EOF) {
			break
		}
		if err != nil {
			return 0, err
		}
	}
	return count, nil
}

func countFilesConcurrently(files []string, workers int) ([]fileCount, error) {
	results := make([]fileCount, len(files))
	if len(files) == 0 {
		return results, nil
	}
	if workers <= 0 {
		workers = 1
	}
	if workers > len(files) {
		workers = len(files)
	}

	errs := make([]error, len(files))
	jobs := make(chan int, len(files))
	for i := range files {
		jobs <- i
	}
	close(jobs)

	var wg sync.WaitGroup
	wg.Add(workers)
	for w := 0; w < workers; w++ {
		go func() {
			defer wg.Done()
			for idx := range jobs {
				n, err := countFileNewlines(files[idx])
				if err != nil {
					errs[idx] = fmt.Errorf("%s: %w", files[idx], err)
					continue
				}
				results[idx] = fileCount{
					path:  files[idx],
					lines: n,
				}
			}
		}()
	}
	wg.Wait()

	for _, err := range errs {
		if err != nil {
			return nil, err
		}
	}
	return results, nil
}

func run(r io.Reader, w io.Writer) error {
	files, err := parseFileList(r)
	if err != nil {
		return err
	}

	results, err := countFilesConcurrently(files, workerCount)
	if err != nil {
		return err
	}

	var allLines int64
	for _, res := range results {
		allLines += res.lines
	}

	sort.SliceStable(results, func(i, j int) bool {
		return results[i].lines > results[j].lines
	})

	bw := bufio.NewWriter(w)
	if _, err := fmt.Fprintf(bw, "All lines: %d\n", allLines); err != nil {
		return err
	}
	for _, res := range results {
		if _, err := fmt.Fprintf(bw, "%d: %s\n", res.lines, res.path); err != nil {
			return err
		}
	}
	return bw.Flush()
}

func main() {
	if err := run(os.Stdin, os.Stdout); err != nil {
		fmt.Fprintf(os.Stderr, "file_line_counter: %v\n", err)
		os.Exit(1)
	}
}
