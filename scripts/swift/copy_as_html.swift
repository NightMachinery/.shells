#!/usr/bin/env swift

// https://github.com/chbrown/macos-pasteboard/issues/8
///

import Cocoa
import Foundation

let pasteboard: NSPasteboard = .general

let dataTypeName : String = "public.html"
let dataType = NSPasteboard.PasteboardType(rawValue: dataTypeName)

let html = "<b>hi</b>"
let data_html = html.data(using: .utf8)!

let dataTypeName_plain : String = "public.utf8-plain-text"
let dataType_plain = NSPasteboard.PasteboardType(rawValue: dataTypeName_plain)

let plain = "hi"
let data_plain = plain.data(using: .utf8)!

// let rtf = """
// {\\rtf1\\ansi\\ansicpg1252\\cocoartf2578
// \\cocoatextscaling0\\cocoaplatform0{\\fonttbl\\f0\\froman\\fcharset0 Times-Roman;}
// {\\colortbl;\\red255\\green255\\blue255;\\red0\\green0\\blue233;\\red0\\green0\\blue0;}
// {\\*\\expandedcolortbl;;\\cssrgb\\c0\\c0\\c93333;\\cssrgb\\c0\\c0\\c0;}
// \\deftab720
// \\pard\\pardeftab720\\partightenfactor0
// {\\field{\\*\\fldinst{HYPERLINK "http://www.google.com/"}}{\\fldrslt
// \\f0\\fs24 \\cf2 \\expnd0\\expndtw0\\kerning0
// \\ul \\ulc2 \\outl0\\strokewidth0 \\strokec2 Click here for Google}}
// \\f0\\fs24 \\cf3 \\expnd0\\expndtw0\\kerning0
// \\outl0\\strokewidth0 \\strokec3  }
// """
// let data_rtf = plain.data(using: .utf8)!
// let dataTypeName_rtf : String = ".rtf"
// let dataType_rtf = NSPasteboard.PasteboardType(rawValue: dataTypeName_rtf)


pasteboard.clearContents() // <-- idk why but this is required prior to setData
pasteboard.setData(data_plain,  forType: dataType_plain)
pasteboard.setData(data_html,  forType: dataType)
// pasteboard.setData(data_rtf,  forType: dataType_rtf)

exit(0)
