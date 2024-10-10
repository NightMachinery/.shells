package main

import (
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"strings"
	"sync"
	"time"
)

func main() {
	// Command-line flags
	bind := flag.String("bind", "127.0.0.1", "IP address to bind to")
	port := flag.String("port", "", "Port to listen on")
	readTimeout := flag.Duration("read-timeout", time.Hour, "Connection read timeout")
	writeTimeout := flag.Duration("write-timeout", time.Hour, "Connection write timeout")
	unidirectional := flag.Bool("u", false, "Unidirectional mode")
	flag.BoolVar(unidirectional, "unidirectional", false, "Unidirectional mode")
	flag.Parse()

	// Check if port is provided
	if *port == "" {
		fmt.Println("Error: --port is required")
		flag.Usage()
		os.Exit(1)
	}

	// Get the command to execute
	args := flag.Args()
	if len(args) == 0 {
		fmt.Println("Error: Command to execute is required")
		flag.Usage()
		os.Exit(1)
	}

	// Parse environment variables from command arguments
	envVars, cmdArgs := parseEnvVars(args)

	// Start listening on the specified TCP address and port
	address := net.JoinHostPort(*bind, *port)
	listener, err := net.Listen("tcp", address)
	if err != nil {
		fmt.Printf("Error: Failed to listen on %s: %v\n", address, err)
		os.Exit(1)
	}
	defer listener.Close()

	fmt.Printf("Listening on %s\n", address)

	for {
		conn, err := listener.Accept()
		if err != nil {
			fmt.Printf("Error: Failed to accept connection: %v\n", err)
			continue
		}
		go handleConnection(conn, cmdArgs, envVars, *readTimeout, *writeTimeout, *unidirectional)
	}
}

func parseEnvVars(args []string) (envVars []string, cmdArgs []string) {
	for i, arg := range args {
		if strings.Contains(arg, "=") {
			envVars = append(envVars, arg)
		} else {
			cmdArgs = args[i:]
			break
		}
	}
	return
}

func handleConnection(conn net.Conn, cmdArgs []string, envVars []string, readTimeout, writeTimeout time.Duration, unidirectional bool) {
	defer conn.Close()
	fmt.Printf("Accepted connection from %s\n", conn.RemoteAddr())

	// Set connection timeouts
	conn.SetReadDeadline(time.Now().Add(readTimeout))
	conn.SetWriteDeadline(time.Now().Add(writeTimeout))

	// Prepare the command
	cmd := exec.Command(cmdArgs[0], cmdArgs[1:]...)
	cmd.Env = append(os.Environ(), envVars...)

	// Set up pipes for stdin, stdout, and stderr
	cmdStdout, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Printf("Error: Failed to get stdout pipe: %v\n", err)
		return
	}
	cmdStderr, err := cmd.StderrPipe()
	if err != nil {
		fmt.Printf("Error: Failed to get stderr pipe: %v\n", err)
		return
	}
	cmdStdin, err := cmd.StdinPipe()
	if err != nil {
		fmt.Printf("Error: Failed to get stdin pipe: %v\n", err)
		return
	}

	// Start the command
	if err := cmd.Start(); err != nil {
		fmt.Printf("Error: Failed to start command: %v\n", err)
		return
	}

	// Use a WaitGroup to ensure all goroutines finish before we exit
	var wg sync.WaitGroup

	// Copy data from connection to command's stdin
	wg.Add(1)
	go func() {
		defer wg.Done()
		io.Copy(cmdStdin, conn)
		cmdStdin.Close() // Close stdin when the connection is closed
	}()

	if unidirectional {
		// In unidirectional mode, pipe the command's output to our own stdout and stderr
		wg.Add(1)
		go func() {
			defer wg.Done()
			io.Copy(os.Stdout, cmdStdout)
		}()
		wg.Add(1)
		go func() {
			defer wg.Done()
			io.Copy(os.Stderr, cmdStderr)
		}()
	} else {
		// In bidirectional mode, pipe the command's output and error back to the connection
		wg.Add(1)
		go func() {
			defer wg.Done()
			io.Copy(conn, cmdStdout)
		}()
		wg.Add(1)
		go func() {
			defer wg.Done()
			io.Copy(conn, cmdStderr)
		}()
	}

	// Wait for all goroutines to finish
	wg.Wait()

	// Wait for the command to finish
	if err := cmd.Wait(); err != nil {
		fmt.Printf("Error: Command exited with error: %v\n", err)
	}

	fmt.Printf("Connection from %s closed\n", conn.RemoteAddr())
}
