#!/usr/bin/env scriptisto

// scriptisto-begin
// script_src: main.swift
// build_cmd: swiftc main.swift -o ./script
// scriptisto-end

// Usage:
// inputlang.swift list
// inputlang.swift com.apple.keylayout.Persian-ISIRI2901

import Carbon

let command = ProcessInfo.processInfo.arguments.dropFirst().last ?? ""
let filter = command == "list" ? nil : [kTISPropertyInputSourceID: command]

guard let cfSources = TISCreateInputSourceList(filter as CFDictionary?, false),
      let sources = cfSources.takeRetainedValue() as? [TISInputSource] else {
    print("Use \"list\" as an argument to list all enabled input sources.")
    exit(-1)
}

if filter == nil { // Print all sources
    print("Change input source by passing one of these names as an argument:")
    sources.forEach {
        let cfID = TISGetInputSourceProperty($0, kTISPropertyInputSourceID)!
        print(Unmanaged<CFString>.fromOpaque(cfID).takeUnretainedValue() as String)
    }
} else if let firstSource = sources.first { // Select this source
    exit(TISSelectInputSource(firstSource))
}
