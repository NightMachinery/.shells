#!/usr/bin/env scriptisto
// @forked from https://github.com/casouri/pbcopyf

// scriptisto-begin
// script_src: main.swift
// build_cmd: swiftc main.swift -o ./script
// scriptisto-end

import Foundation
import Cocoa
#if os(Linux)
    import Glibc
#else
    import Darwin
#endif

func getFilesFromPasteboard() -> [String]? {
    // get items
    let pasteboard = NSPasteboard.general
    guard let items =  pasteboard.pasteboardItems else {
        print("No files in pasteboard")
        return nil
    }
    // transform to file paths
    var filePathArray: [String] = []
    for item in items {
        let string = item.string(forType: NSPasteboard.PasteboardType("public.file-url"))
        if string != nil {
            let path = removePrefix(self: string!,prefix: "file://").removingPercentEncoding!
            filePathArray.append(path)
        }
    }
    return filePathArray
}

func removePrefix(self: String, prefix: String) -> String {
    guard self.hasPrefix(prefix) else { return self }
    return String(self.dropFirst(prefix.count))
}

func main() {
    // do {
        guard let filePathArray = getFilesFromPasteboard() else {
            return
        }

        for path in filePathArray {
            print(path)
        }
    // } catch {
    //     fputs(error.localizedDescription + "\n", stderr)
    // }
}

main()
