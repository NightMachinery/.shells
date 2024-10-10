package main

import (
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"regexp"
	"strings"
	"sync"
	"time"

	"golang.design/x/clipboard"
)

func main() {
	// Init returns an error if the package is not ready for use.
	err := clipboard.Init()
	if err != nil {
		panic(err)
	}

	// Command-line flags
	bind := flag.String("bind", "127.0.0.1", "IP address to bind to")
	port := flag.String("port", "", "Port to listen on")
	readTimeout := flag.Duration("read-timeout", time.Hour, "Connection read timeout")
	writeTimeout := flag.Duration("write-timeout", time.Hour, "Connection write timeout")
	unidirectional := flag.Bool("u", false, "Unidirectional mode")
	flag.BoolVar(unidirectional, "unidirectional", false, "Unidirectional mode")
	verbose := flag.Int("verbose", 2, "Set verbosity level")
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

	// Check if the command is MAGIC_COPY
	isMagicCopy := false
	if len(cmdArgs) > 0 && cmdArgs[0] == "MAGIC_COPY" {
		isMagicCopy = true
	}

	// Start listening on the specified TCP address and port
	address := net.JoinHostPort(*bind, *port)
	listener, err := net.Listen("tcp", address)
	if err != nil {
		fmt.Printf("Error: Failed to listen on %s: %v\n", address, err)
		os.Exit(1)
	}
	defer listener.Close()

	if *verbose > 0 {
		fmt.Printf("Listening on %s\n", address)
	}

	for {
		conn, err := listener.Accept()
		if err != nil {
			if *verbose > 0 {
				fmt.Printf("Error: Failed to accept connection: %v\n", err)
			}
			continue
		}
		go handleConnection(conn, cmdArgs, envVars, *readTimeout, *writeTimeout, *unidirectional, isMagicCopy, *verbose)
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

func handleConnection(conn net.Conn, cmdArgs []string, envVars []string, readTimeout, writeTimeout time.Duration, unidirectional bool, isMagicCopy bool, verbose int) {
	defer conn.Close()
	if verbose > 0 {
		fmt.Printf("Accepted connection from %s\n", conn.RemoteAddr())
	}

	// Set connection timeouts
	conn.SetReadDeadline(time.Now().Add(readTimeout))
	conn.SetWriteDeadline(time.Now().Add(writeTimeout))

	if isMagicCopy {
		// Read data from the connection
		var data strings.Builder
		_, err := io.Copy(&data, conn)
		if err != nil {
			if verbose > 0 {
				fmt.Printf("Error: Failed to read data from connection: %v\n", err)
			}
			return
		}

		receivedText := data.String()
		receivedTextTrimmed := strings.TrimSpace(receivedText)

		// Check if the data matches MAGIC_BELL_ regex
		re := regexp.MustCompile(`^MAGIC_BELL_(bell\S?-?\S*)$`)
		matches := re.FindStringSubmatch(receivedTextTrimmed)

		if len(matches) == 2 {
			// Extract bell name and execute command
			bellName := matches[1]

			// Log the action
			if verbose > 0 {
				fmt.Printf("Received MAGIC_BELL command with bell name: %s\n", bellName)
			}

			// Execute brishzq.zsh <bellName>
			cmd := exec.Command("brishzq.zsh", bellName)

			// Start the command
			if err := cmd.Start(); err != nil {
				if verbose > 0 {
					fmt.Printf("Error: Failed to start command: %v\n", err)
				}
				return
			}

			// Wait for the command to finish
			if err := cmd.Wait(); err != nil {
				if verbose > 0 {
					fmt.Printf("Error: Command exited with error: %v\n", err)
				}
			}

			// Log the action
			if verbose > 0 {
				fmt.Printf("Executed brishzq.zsh %s\n", bellName)
			}

			if verbose > 0 {
				fmt.Printf("Connection from %s closed\n", conn.RemoteAddr())
			}
			return
		}

		// If not MAGIC_BELL, copy data to clipboard
		clipboard.Write(clipboard.FmtText, []byte(receivedText))

		// Log data if verbosity level is greater than 1
		if verbose > 1 {
			fmt.Printf("Data copied to clipboard: %s\n", receivedText)
		}

		if verbose > 0 {
			fmt.Printf("Connection from %s closed\n", conn.RemoteAddr())
		}
		return
	}

	// Prepare the command
	cmd := exec.Command(cmdArgs[0], cmdArgs[1:]...)
	cmd.Env = append(os.Environ(), envVars...)

	// Set up pipes for stdin, stdout, and stderr
	cmdStdout, err := cmd.StdoutPipe()
	if err != nil {
		if verbose > 0 {
			fmt.Printf("Error: Failed to get stdout pipe: %v\n", err)
		}
		return
	}
	cmdStderr, err := cmd.StderrPipe()
	if err != nil {
		if verbose > 0 {
			fmt.Printf("Error: Failed to get stderr pipe: %v\n", err)
		}
		return
	}
	cmdStdin, err := cmd.StdinPipe()
	if err != nil {
		if verbose > 0 {
			fmt.Printf("Error: Failed to get stdin pipe: %v\n", err)
		}
		return
	}

	// Start the command
	if err := cmd.Start(); err != nil {
		if verbose > 0 {
			fmt.Printf("Error: Failed to start command: %v\n", err)
		}
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
		if verbose > 0 {
			fmt.Printf("Error: Command exited with error: %v\n", err)
		}
	}

	if verbose > 0 {
		fmt.Printf("Connection from %s closed\n", conn.RemoteAddr())
	}
}
