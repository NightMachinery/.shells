// swiftc -O -framework ApplicationServices -framework AppKit get_selected_text.swift -o getsel
//
// You need to add the binary to Accessibility permissions in System Preferences.
//
// This works, but only for apps that implement the AX API. Telegram does not.
//
// * @usage
// `sleep 2 ; getsel --debug ; bell-lm-lets-finish-this`
///
import Foundation
import ApplicationServices
import AppKit

// ---------- Portable constant helpers (AX constants import variations) ----------
@inline(__always) func AXAttr(_ s: String) -> CFString { s as CFString }
@inline(__always) func AXAttr(_ u: Unmanaged<CFString>) -> CFString { u.takeUnretainedValue() }
@inline(__always) func AXKey(_ s: String) -> String { s }
@inline(__always) func AXKey(_ u: Unmanaged<CFString>) -> String { u.takeUnretainedValue() as String }

// ---------- Normalized constants ----------
let K_FOCUSED            = AXAttr(kAXFocusedUIElementAttribute)
let K_FOCUSED_APP        = AXAttr(kAXFocusedApplicationAttribute)
let K_FOCUSED_WINDOW     = AXAttr(kAXFocusedWindowAttribute)
let K_MAIN_WINDOW        = AXAttr(kAXMainWindowAttribute)
let K_WINDOWS            = AXAttr(kAXWindowsAttribute)
let K_SELECTED           = AXAttr(kAXSelectedTextAttribute)
let K_SEL_RANGE          = AXAttr(kAXSelectedTextRangeAttribute)
let K_STR_FOR_RANGE      = AXAttr(kAXStringForRangeParameterizedAttribute)
let K_VALUE              = AXAttr(kAXValueAttribute)
let K_ROLE               = AXAttr(kAXRoleAttribute)
let K_SUBROLE            = AXAttr(kAXSubroleAttribute)
let K_TITLE              = AXAttr(kAXTitleAttribute)
let K_PARENT             = AXAttr(kAXParentAttribute)
let K_CHILDREN           = AXAttr(kAXChildrenAttribute)

// ---------- Args ----------
struct Args { var waitSeconds: Double = 0; var debug = false; var dumpAttrs = false; var maxNodes = 9000 }
func parseArgs() -> Args {
    var a = Args(); var it = CommandLine.arguments.dropFirst().makeIterator()
    while let arg = it.next() {
        switch arg {
        case "--wait","-w": if let s = it.next(), let d = Double(s) { a.waitSeconds = d }
        case "--debug","-d": a.debug = true
        case "--attrs","-a": a.dumpAttrs = true
        case "--maxnodes","-m": if let s = it.next(), let n = Int(s) { a.maxNodes = max(20, n) }
        default: break
        }
    }
    return a
}

// ---------- Logging & helpers ----------
@inline(__always) func log(_ s: String, enabled: Bool = true) { if enabled { fputs(s + "\n", stderr) } }

func axCopy(_ elem: AXUIElement, _ attr: CFString) -> (AXError, CFTypeRef?) {
    var out: CFTypeRef? = nil
    let err = AXUIElementCopyAttributeValue(elem, attr, &out)
    return (err, out)
}
func axCopyParam(_ elem: AXUIElement, _ attr: CFString, _ param: CFTypeRef) -> (AXError, CFTypeRef?) {
    var out: CFTypeRef? = nil
    let err = AXUIElementCopyParameterizedAttributeValue(elem, attr, param, &out)
    return (err, out)
}
func errName(_ e: AXError) -> String {
    switch e {
    case .success:                              return "success"
    case .noValue:                              return "noValue"
    case .attributeUnsupported:                 return "attributeUnsupported"
    case .actionUnsupported:                    return "actionUnsupported"
    case .notificationUnsupported:              return "notificationUnsupported"
    case .notificationAlreadyRegistered:        return "notificationAlreadyRegistered"
    case .notificationNotRegistered:            return "notificationNotRegistered"
    case .apiDisabled:                          return "apiDisabled"
    case .parameterizedAttributeUnsupported:    return "parameterizedAttributeUnsupported"
    case .notEnoughPrecision:                   return "notEnoughPrecision"
    case .notImplemented:                       return "notImplemented"
    case .cannotComplete:                       return "cannotComplete"
    case .failure:                              return "failure"
    case .illegalArgument:                      return "illegalArgument"
    case .invalidUIElement:                     return "invalidUIElement"
    case .invalidUIElementObserver:             return "invalidUIElementObserver"
    @unknown default:                           return "unknown(\(e.rawValue))"
    }
}


func isTrusted(prompt: Bool = true) -> Bool {
    AXIsProcessTrustedWithOptions([AXKey(kAXTrustedCheckOptionPrompt): prompt] as CFDictionary)
}

func frontmostNSApp() -> NSRunningApplication? { NSWorkspace.shared.frontmostApplication }

func appInfo(pid: pid_t) -> (name: String, bid: String) {
    if let ra = NSRunningApplication(processIdentifier: pid) {
        return (ra.localizedName ?? "?", ra.bundleIdentifier ?? "?")
    }
    return ("?", "?")
}

// ---------- AX utilities ----------
func attrNames(of elem: AXUIElement) -> [String] {
    var namesRef: CFArray?
    guard AXUIElementCopyAttributeNames(elem, &namesRef) == .success,
          let arr = namesRef as? [CFString] else { return [] }
    return arr.map { $0 as String }
}
func strAttr(_ elem: AXUIElement, _ attr: CFString) -> String? {
    let (e, ref) = axCopy(elem, attr)
    guard e == .success else { return nil }
    return ref as? String
}
func elemInfo(_ e: AXUIElement) -> String {
    var pid: pid_t = 0; AXUIElementGetPid(e, &pid)
    let (n,b) = appInfo(pid: pid)
    let role = strAttr(e, K_ROLE) ?? "?"
    let sub  = strAttr(e, K_SUBROLE) ?? "-"
    let ttl  = strAttr(e, K_TITLE) ?? ""
    return "pid=\(pid) app=\"\(n)\" (\(b)) role=\(role) subrole=\(sub) title=\"\(ttl)\""
}

// Returns (text, pathDescription)
func selectedText(from elem: AXUIElement) -> (String?, String) {
    // 1) AXSelectedText
    let (e1, v1) = axCopy(elem, K_SELECTED)
    if e1 == .success, let s = v1 as? String, !s.isEmpty { return (s, "AXSelectedText") }

    // 2) AXSelectedTextRange → AXStringForRange
    let (e2, v2) = axCopy(elem, K_SEL_RANGE)
    if e2 == .success, let rr = v2, CFGetTypeID(rr) == AXValueGetTypeID() {
        let rangeVal = rr as! AXValue
        var cfRange = CFRange(); if AXValueGetValue(rangeVal, .cfRange, &cfRange) {
            let (e3, out) = axCopyParam(elem, K_STR_FOR_RANGE, rangeVal)
            if e3 == .success, let s = out as? String, !s.isEmpty {
                return (s, "AXSelectedTextRange→AXStringForRange")
            }
            // 2b) Fallback slice of AXValue
            let (e4, v4) = axCopy(elem, K_VALUE)
            if e4 == .success, let full = v4 as? String,
               cfRange.location >= 0, cfRange.length >= 0,
               cfRange.location + cfRange.length <= full.utf16.count {
                let startUTF16 = full.utf16.index(full.utf16.startIndex, offsetBy: cfRange.location)
                let endUTF16   = full.utf16.index(startUTF16, offsetBy: cfRange.length)
                if let start = String.Index(startUTF16, within: full),
                   let end   = String.Index(endUTF16, within: full) {
                    return (String(full[start..<end]), "AXSelectedTextRange→slice(AXValue)")
                }
            }
        }
    }
    return (nil, "no-selection")
}

// BFS around a seed node. Returns (text, visitedCount, detail)
func bfsFindSelection(start: AXUIElement, cap: Int) -> (String?, Int, String) {
    var visited = Set<UInt>()
    func idFor(_ e: AXUIElement) -> UInt { UInt(bitPattern: Unmanaged.passUnretained(e).toOpaque()) }

    var q: [AXUIElement] = [start]
    var i = 0
    while i < q.count && i < cap {
        let e = q[i]; i += 1
        let key = idFor(e); if visited.contains(key) { continue }
        visited.insert(key)

        let (txt, how) = selectedText(from: e)
        if let s = txt, !s.isEmpty {
            return (s, visited.count, "Found at \(elemInfo(e)) via \(how)")
        }

        // parent
        let (pe, pref) = axCopy(e, K_PARENT)
        if pe == .success, let p = pref as! AXUIElement? { q.append(p) }
        // children
        let (ce, cref) = axCopy(e, K_CHILDREN)
        if ce == .success, let arr = cref as? [AXUIElement] { q.append(contentsOf: arr) }
    }
    return (nil, visited.count, "BFS exhausted \(visited.count) nodes")
}

// Try to get a seed: system focus → AX focused app → NSWorkspace frontmost app
func obtainSeed(debug: Bool) -> (AXUIElement?, String) {
    let system = AXUIElementCreateSystemWide()

    // (a) System focused element
    do {
        let (err, ref) = axCopy(system, K_FOCUSED)
        log("AX system kAXFocusedUIElement -> \(errName(err))", enabled: debug)
        if err == .success, let e = ref as! AXUIElement? {
            return (e, "Seed via system focused element: \(elemInfo(e))")
        }
    }

    // (b) System focused application
    do {
        let (err, aref) = axCopy(system, K_FOCUSED_APP)
        log("AX system kAXFocusedApplication -> \(errName(err))", enabled: debug)
        if err == .success, let app = aref as! AXUIElement? {
            let info = "Focused AX application: \(elemInfo(app))"
            // drill into focused element/window
            let (e1, r1) = axCopy(app, K_FOCUSED)
            log("  app kAXFocusedUIElement -> \(errName(e1))", enabled: debug)
            if e1 == .success, let e = r1 as! AXUIElement? { return (e, info + " → focused element: \(elemInfo(e))") }

            let (e2, r2) = axCopy(app, K_FOCUSED_WINDOW)
            log("  app kAXFocusedWindow -> \(errName(e2))", enabled: debug)
            if e2 == .success, let w = r2 as! AXUIElement? { return (w, info + " → focused window: \(elemInfo(w))") }

            let (e3, r3) = axCopy(app, K_MAIN_WINDOW)
            log("  app kAXMainWindow -> \(errName(e3))", enabled: debug)
            if e3 == .success, let w = r3 as! AXUIElement? { return (w, info + " → main window: \(elemInfo(w))") }

            let (e4, r4) = axCopy(app, K_WINDOWS)
            log("  app kAXWindows -> \(errName(e4))", enabled: debug)
            if e4 == .success, let ws = r4 as? [AXUIElement], let first = ws.first {
                return (first, info + " → first window: \(elemInfo(first))")
            }
        }
    }

    // (c) NSWorkspace frontmost app → AX application by PID
    if let nsApp = frontmostNSApp() {
        let pid = nsApp.processIdentifier
        let appAX = AXUIElementCreateApplication(pid)
        let info = "NSWorkspace frontmost app → AX application: pid=\(pid) app=\"\(nsApp.localizedName ?? "?")\" (\(nsApp.bundleIdentifier ?? "?"))"
        // Try same chain on this AX app
        let (e1, r1) = axCopy(appAX, K_FOCUSED)
        log("  nsapp kAXFocusedUIElement -> \(errName(e1))", enabled: debug)
        if e1 == .success, let e = r1 as! AXUIElement? { return (e, info + " → focused element: \(elemInfo(e))") }

        let (e2, r2) = axCopy(appAX, K_FOCUSED_WINDOW)
        log("  nsapp kAXFocusedWindow -> \(errName(e2))", enabled: debug)
        if e2 == .success, let w = r2 as! AXUIElement? { return (w, info + " → focused window: \(elemInfo(w))") }

        let (e3, r3) = axCopy(appAX, K_MAIN_WINDOW)
        log("  nsapp kAXMainWindow -> \(errName(e3))", enabled: debug)
        if e3 == .success, let w = r3 as! AXUIElement? { return (w, info + " → main window: \(elemInfo(w))") }

        let (e4, r4) = axCopy(appAX, K_WINDOWS)
        log("  nsapp kAXWindows -> \(errName(e4))", enabled: debug)
        if e4 == .success, let ws = r4 as? [AXUIElement], let first = ws.first {
            return (first, info + " → first window: \(elemInfo(first))")
        }
    } else {
        log("NSWorkspace.frontmostApplication: <nil>", enabled: debug)
    }

    return (nil, "Couldn’t obtain a focused element/window from AX or NSWorkspace.")
}

// ---------- Main ----------
let args = parseArgs()

let trusted = isTrusted(prompt: true)
log("Accessibility trusted: \(trusted ? "YES" : "NO")")
if let a = frontmostNSApp() {
    log("Frontmost NSWorkspace app: \"\(a.localizedName ?? "?")\" (\(a.bundleIdentifier ?? "?")) pid=\(a.processIdentifier)")
} else {
    log("Frontmost NSWorkspace app: <none>")
}

if !trusted {
    fputs("Accessibility not granted. Enable this exact binary in Settings → Privacy & Security → Accessibility.\n", stderr)
    exit(EXIT_FAILURE)
}

if args.waitSeconds > 0 {
    log("Waiting \(args.waitSeconds)s… (switch to target app and make a selection)")
    Thread.sleep(forTimeInterval: args.waitSeconds)
}

let (seedOpt, whereSeed) = obtainSeed(debug: args.debug)
guard let seed = seedOpt else {
    fputs(whereSeed + "\n", stderr)
    exit(EXIT_FAILURE)
}
log("Seed: \(whereSeed)")

if args.dumpAttrs {
    let names = attrNames(of: seed).sorted()
    log("Seed attributes (\(names.count)): \(names.joined(separator: ", "))")
}

// Try seed
let (txt, how) = selectedText(from: seed)
if let s = txt, !s.isEmpty {
    log("Selection obtained from seed via: \(how)")
    print(s)
    exit(EXIT_SUCCESS)
}

// BFS
log("No selection on seed; scanning nearby (maxNodes=\(args.maxNodes))…")
let (s2, visited, detail) = bfsFindSelection(start: seed, cap: args.maxNodes)
log(detail)
log("BFS visited \(visited) nodes.")
if let s = s2 {
    print(s)
    exit(EXIT_SUCCESS)
}

fputs("No selection found (nothing selected, secure field, or the app doesn’t expose selection).\n", stderr)
exit(EXIT_FAILURE)
