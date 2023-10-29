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

enum FileError: Error {
    case PathInvalid(String)
    case FileNotFound(String)
    case FileCannotWrite(String)
    case FileCannotDelete(String)
    case FileCannotRead(String)
}

enum ProgramError: Error {
    case NotEnoughArguments(String)
    case Terminate
}

enum PasteboardError: Error {
    case FailedToWritePasteboard
}

func putFilesToPasteboard(text: String, files fileArray: [String]) throws {
    /// check for possible errors
    for path in fileArray {
        if !FileManager.default.fileExists(atPath: path) {
            throw FileError.FileNotFound(path)
        }
    }

    // create items
    let pasteboardItems = fileArray.map { path -> NSPasteboardItem in
        let item = NSPasteboardItem()
        let pathURL = URL(fileURLWithPath: path)

        item.setString(pathURL.absoluteString, forType: NSPasteboard.PasteboardType("public.file-url"))
        item.setString(text, forType: NSPasteboard.PasteboardType("public.utf8-plain-text"))
        return item
    }

    // write to pasteboard
    NSPasteboard.general.clearContents() // you have to clear before write
    if !NSPasteboard.general.writeObjects(pasteboardItems) {
        throw PasteboardError.FailedToWritePasteboard
    }
}

func main() {
    do {
        guard CommandLine.arguments.count > 2 else {
            throw ProgramError.NotEnoughArguments("Requires <text> <file-path> arguments")
        }

        let text = CommandLine.arguments[1]
        var pathArray = Array(CommandLine.arguments.dropFirst(2))
        pathArray = pathArray.map {path -> String in
            return URL(fileURLWithPath: path).standardizedFileURL.path
        }

        try putFilesToPasteboard(text: text, files: pathArray)
    } catch {
        fputs(error.localizedDescription + "\n", stderr)
    }
}

main()
