#!/usr/bin/env swift

import Foundation

class ScreenLockObserver {
    let verbose: Bool

    init(verbose: Bool) {
        self.verbose = verbose
        self.log("Lock Watcher Started")

        let dnc = DistributedNotificationCenter.default()

        // listen for screen lock
        let _ = dnc.addObserver(forName: NSNotification.Name("com.apple.screenIsLocked"), object: nil, queue: .main) { _ in
            self.log("Screen Locked")
            self.runZshScript(script: "lock")
        }

        // listen for screen unlock
        let _ = dnc.addObserver(forName: NSNotification.Name("com.apple.screenIsUnlocked"), object: nil, queue: .main) { _ in
            self.log("Screen Unlocked")
            self.runZshScript(script: "unlock")
        }

        // Set up SIGINT handler
        signal(SIGINT) { _ in
            print("\nReceived interrupt signal. Exiting...")
            exit(0)
        }

        RunLoop.main.run()
    }

    private func runZshScript(script: String) {
        guard let nightDir = ProcessInfo.processInfo.environment["NIGHTDIR"] else {
            self.log("Error: NIGHTDIR environment variable not set")
            return
        }

        let path = "\(nightDir)/zshlang/hooks/\(script).zsh"
        self.log("Running script: \(path)")

        let task = Process()
        task.launchPath = "/bin/zsh"
        task.arguments = ["-f", path]
        task.launch()
        task.waitUntilExit()
    }

    private func log(_ message: String) {
        if verbose {
            print(message)
        }
        // NSLog(message)
    }
}

let verbose = CommandLine.arguments.contains("-v")
let _ = ScreenLockObserver(verbose: verbose)
