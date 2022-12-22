#!/usr/bin/env scriptisto

// scriptisto-begin
// script_src: main.swift
// build_cmd: swiftc main.swift -o ./script
// scriptisto-end

// Usage:
//  clipboard_changed.swift
//  It will output changed texts with a NUL separator.
///
import Foundation
import Cocoa
#if os(Linux)
    import Glibc
#else
    import Darwin.C
#endif

func WatchPasteboard(copied: @escaping (_ copiedString:String) -> Void) {
    let pasteboard = NSPasteboard.general
    var changeCount = NSPasteboard.general.changeCount
    Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { _ in
        if let copiedString = pasteboard.string(forType: .string) {
            if pasteboard.changeCount != changeCount {
                copied(copiedString)
                changeCount = pasteboard.changeCount
            }
        }
    }
}

WatchPasteboard { copiedString in
    // print("copied: ")
    print(copiedString, terminator: "\0")
    fflush(stdout)
}

RunLoop.main.run()
