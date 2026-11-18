// Agent Desk Tabs: switches a browser tab that is already on the agent-desk
// dashboard to the item a notification click names, instead of opening a new
// tab. It is the only program that needs the Automation permission to
// control the browser, and it can do one thing with it: move a dashboard tab
// to another dashboard page. Its AppleScript is fixed here, the dashboard's
// address comes from agent-desk's config file (never from the caller), and a
// URL outside that address is refused. With no such tab, no permission, or a
// browser it does not know, it opens the URL as usual.
//
//   open -n -g -a "Agent Desk Tabs" --args URL
//   "Agent Desk Tabs.app/Contents/MacOS/agent-desk-tabs" --dry-run URL   # what it would do
//
// See docs/agent-desk.md in ~/scripts.

import AppKit
import Foundation

// Chromium-family browsers: their AppleScript lists each tab's URL.
let tabBrowsers = [
    "com.brave.Browser", "com.google.Chrome", "com.google.Chrome.beta", "com.google.Chrome.canary",
    "org.chromium.Chromium", "com.microsoft.edgemac", "com.vivaldi.Vivaldi",
]

func note(_ s: String) {
    // To stderr and the unified log: log show --predicate 'process == "agent-desk-tabs"'
    NSLog("%@", s)
}

// base_url from ~/.config/agent-desk/config.toml (the top-level key).
func baseURL() -> String? {
    let env = ProcessInfo.processInfo.environment
    var path = env["AGENT_DESK_CONFIG"] ?? ""
    if path.isEmpty {
        let dir = env["XDG_CONFIG_HOME"] ?? (NSHomeDirectory() + "/.config")
        path = dir + "/agent-desk/config.toml"
    }
    guard let text = try? String(contentsOfFile: path, encoding: .utf8) else { return nil }
    for raw in text.split(separator: "\n") {
        let line = raw.trimmingCharacters(in: .whitespaces)
        if line.hasPrefix("[") { break } // tables come after the top-level keys
        guard line.hasPrefix("base_url") else { continue }
        let parts = line.split(separator: "=", maxSplits: 1)
        guard parts.count == 2, parts[0].trimmingCharacters(in: .whitespaces) == "base_url" else { continue }
        var v = parts[1].trimmingCharacters(in: .whitespaces)
        if let hash = v.range(of: " #") { v = String(v[..<hash.lowerBound]).trimmingCharacters(in: .whitespaces) }
        guard v.count >= 2, let q = v.first, (q == "\"" || q == "'"), v.last == q else { return nil }
        return String(v.dropFirst().dropLast())
    }
    return nil
}

// The tab-switching browser: the default https handler, when it is a known one.
func browserID() -> String? {
    guard let probe = URL(string: "https://example.com"),
          let app = NSWorkspace.shared.urlForApplication(toOpen: probe),
          let id = Bundle(url: app)?.bundleIdentifier else { return nil }
    return tabBrowsers.first { $0.lowercased() == id.lowercased() }
}

func appleScriptString(_ s: String) -> String {
    "\"" + s.replacingOccurrences(of: "\\", with: "\\\\").replacingOccurrences(of: "\"", with: "\\\"") + "\""
}

enum Outcome { case reused, noTab, failed(String) }

func reuseTab(browser: String, base: String, target: String) -> Outcome {
    let source = """
    tell application id \(appleScriptString(browser))
        repeat with w in windows
            set i to 0
            repeat with t in tabs of w
                set i to i + 1
                if (URL of t) starts with \(appleScriptString(base)) then
                    set URL of t to \(appleScriptString(target))
                    set active tab index of w to i
                    set index of w to 1
                    activate
                    return "reused"
                end if
            end repeat
        end repeat
    end tell
    return "none"
    """
    var err: NSDictionary?
    guard let script = NSAppleScript(source: source) else { return .failed("cannot build the script") }
    let res = script.executeAndReturnError(&err)
    if let err = err { return .failed("\(err[NSAppleScript.errorNumber] ?? "?"): \(err[NSAppleScript.errorMessage] ?? "")") }
    return res.stringValue == "reused" ? .reused : .noTab
}

func openNormally(_ url: URL) {
    NSWorkspace.shared.open(url)
}

var args = Array(CommandLine.arguments.dropFirst())
let dry = args.first == "--dry-run"
if dry { args.removeFirst() }
guard args.count == 1, let target = URL(string: args[0]), let scheme = target.scheme?.lowercased(),
      scheme == "https" || scheme == "http" else {
    note("agent-desk-tabs: usage: agent-desk-tabs [--dry-run] URL (an http or https URL)")
    exit(2)
}
guard let rawBase = baseURL(), !rawBase.isEmpty else {
    note("agent-desk-tabs: no base_url in agent-desk's config; refusing \(args[0])")
    exit(3)
}
// The dashboard's address, up to (not including) the # of its routes.
let base = String(rawBase.split(separator: "#", maxSplits: 1, omittingEmptySubsequences: false)[0])
guard args[0].hasPrefix(base) else {
    note("agent-desk-tabs: refused: \(args[0]) is not on the dashboard (\(base))")
    exit(3)
}
guard let browser = browserID(), !NSRunningApplication.runningApplications(withBundleIdentifier: browser).isEmpty else {
    if dry { print("open: the default browser is not a running tab browser") ; exit(0) }
    openNormally(target)
    exit(0)
}
if dry { print("reuse: \(browser), tabs starting with \(base)"); exit(0) }
switch reuseTab(browser: browser, base: base, target: args[0]) {
case .reused:
    exit(0)
case .noTab:
    openNormally(target)
case .failed(let why):
    note("agent-desk-tabs: \(browser): \(why); opening a new tab")
    openNormally(target)
}
exit(0)
