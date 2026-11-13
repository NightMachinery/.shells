#!/usr/bin/env scriptisto

// scriptisto-begin
// script_src: main.swift
// build_cmd: swiftc -O main.swift -o ./script
// scriptisto-end

// Standalone macOS helper + Kitty RC fast path for `kitty-popup-window-toggle'.
// Mirrors the panel-mode behavior of hammerspoon/core/window-media-bindings.lua,
// hammerspoon/core/kitty-panel.lua, and hammerspoon/core/screen-decor.lua
// without talking to Hammerspoon.

import AppKit
import ApplicationServices
import Darwin
import Foundation

let kittyBundleID = "net.kovidgoyal.kitty"
let kittyAppPath = "/Applications/kitty.app"
let kittyPanelClass = "kitty-panel"
let screenDecorBundleID = "night.screen-decor"
let screenDecorCoverNote = Notification.Name("night.screen-decor.cover")

let transientBundleIDs: Set<String> = [
    "org.hammerspoon.Hammerspoon",
    "org.p0deje.Maccy",
    "com.pais.handy",
    "night.screen-decor",
]

let kMenuBarLayer = 24
let kMinNormalW: CGFloat = 100
let kMinNormalH: CGFloat = 60
let kPanelMinDim: CGFloat = 200
let kPanelFitSlack: CGFloat = 2.0

let exitNeedsLaunch: Int32 = 10
let exitNeedsSlowShow: Int32 = 11
let exitNeedsSlowHide: Int32 = 12

func activateApp(_ app: NSRunningApplication) {
    if #available(macOS 14.0, *) {
        app.activate()
    } else {
        app.activate(options: .activateIgnoringOtherApps)
    }
}

func logErr(_ msg: String) {
    FileHandle.standardError.write(("kitty_panel: " + msg + "\n").data(using: .utf8)!)
}

func defaultNightDir() -> String {
    if let env = ProcessInfo.processInfo.environment["NIGHTDIR"], !env.isEmpty {
        return env
    }
    return NSHomeDirectory() + "/scripts"
}

func defaultSocketsDir() -> String {
    if let env = ProcessInfo.processInfo.environment["NIGHT_SOCKETS_DIR"], !env.isEmpty {
        return env
    }
    return NSHomeDirectory() + "/.local/state"
}

// MARK: - Private CoreGraphics / Accessibility symbols

typealias CGSMainConnectionIDFn = @convention(c) () -> Int32
typealias CGSCopyManagedDisplaySpacesFn = @convention(c) (Int32) -> CFArray?
typealias CGSSpaceGetTypeFn = @convention(c) (Int32, Int64) -> Int32
typealias CGSMoveWindowsToManagedSpaceFn = @convention(c) (Int32, CFArray, Int64) -> Void
typealias AXUIElementGetWindowFn = @convention(c) (AXUIElement, UnsafeMutablePointer<CGWindowID>) -> AXError

struct PrivateAPIs {
    let mainConnectionID: CGSMainConnectionIDFn?
    let copyManagedDisplaySpaces: CGSCopyManagedDisplaySpacesFn?
    let spaceGetType: CGSSpaceGetTypeFn?
    let moveWindowsToManagedSpace: CGSMoveWindowsToManagedSpaceFn?
    let axGetWindow: AXUIElementGetWindowFn?

    static let shared: PrivateAPIs = {
        let handle = dlopen(nil, RTLD_LAZY)
        func sym<T>(_ name: String) -> T? {
            guard let ptr = dlsym(handle, name) else { return nil }
            return unsafeBitCast(ptr, to: T.self)
        }
        return PrivateAPIs(
            mainConnectionID: sym("CGSMainConnectionID"),
            copyManagedDisplaySpaces: sym("CGSCopyManagedDisplaySpaces"),
            spaceGetType: sym("CGSSpaceGetType"),
            moveWindowsToManagedSpace: sym("CGSMoveWindowsToManagedSpace"),
            axGetWindow: sym("_AXUIElementGetWindow")
        )
    }()
}

// MARK: - Screens & CoreGraphics Window Stack

struct ScreenInfo {
    let nsScreen: NSScreen
    let cgid: CGDirectDisplayID
    let uuid: String
    let name: String
    let fullFrame: CGRect
    let usableFrame: CGRect

    var expectedPanelFrame: CGRect {
        let bottom = fullFrame.origin.y + fullFrame.size.height
        return CGRect(
            x: fullFrame.origin.x,
            y: usableFrame.origin.y,
            width: fullFrame.size.width,
            height: max(0, bottom - usableFrame.origin.y)
        )
    }
}

struct WindowEntry {
    let id: CGWindowID
    let pid: pid_t
    let layer: Int
    let alpha: Double
    let frame: CGRect

    var isNormal: Bool {
        layer == 0 && alpha > 0 && frame.width >= kMinNormalW && frame.height >= kMinNormalH
    }

    var isFloatingNormalSized: Bool {
        layer > 0 && layer < kMenuBarLayer && alpha > 0 && frame.width >= kMinNormalW && frame.height >= kMinNormalH
    }

    var isPanelSized: Bool {
        layer != 0 && alpha > 0 && frame.width > kPanelMinDim && frame.height > kPanelMinDim
    }
}

func appKitToCG(_ r: NSRect, primaryHeight: CGFloat) -> CGRect {
    CGRect(
        x: r.origin.x,
        y: primaryHeight - (r.origin.y + r.size.height),
        width: r.size.width,
        height: r.size.height
    )
}

func screenUUID(for cgid: CGDirectDisplayID) -> String {
    guard let cfUUID = CGDisplayCreateUUIDFromDisplayID(cgid)?.takeRetainedValue() else {
        return ""
    }
    return (CFUUIDCreateString(nil, cfUUID) as String).uppercased()
}

func readScreens() -> [ScreenInfo] {
    let nsScreens = NSScreen.screens
    let primaryHeight = nsScreens.first?.frame.height ?? 0
    return nsScreens.map { s in
        let num = (s.deviceDescription[NSDeviceDescriptionKey("NSScreenNumber")] as? NSNumber)?.uint32Value ?? 0
        let cgid = CGDirectDisplayID(num)
        return ScreenInfo(
            nsScreen: s,
            cgid: cgid,
            uuid: screenUUID(for: cgid),
            name: s.localizedName,
            fullFrame: appKitToCG(s.frame, primaryHeight: primaryHeight),
            usableFrame: appKitToCG(s.visibleFrame, primaryHeight: primaryHeight)
        )
    }
}

func readWindowStack() -> [WindowEntry] {
    let opts: CGWindowListOption = [.optionOnScreenOnly, .excludeDesktopElements]
    guard let list = CGWindowListCopyWindowInfo(opts, kCGNullWindowID) as? [[String: Any]] else {
        return []
    }
    var out: [WindowEntry] = []
    out.reserveCapacity(list.count)
    for w in list {
        guard
            let wid = (w[kCGWindowNumber as String] as? NSNumber)?.uint32Value,
            let pid = (w[kCGWindowOwnerPID as String] as? NSNumber)?.int32Value,
            let boundsDict = w[kCGWindowBounds as String] as? NSDictionary,
            let frame = CGRect(dictionaryRepresentation: boundsDict)
        else { continue }
        let layer = (w[kCGWindowLayer as String] as? NSNumber)?.intValue ?? 0
        let alpha = (w[kCGWindowAlpha as String] as? NSNumber)?.doubleValue ?? 1.0
        out.append(WindowEntry(id: wid, pid: pid, layer: layer, alpha: alpha, frame: frame))
    }
    return out
}

func screenOfFrame(_ frame: CGRect, screens: [ScreenInfo]) -> ScreenInfo? {
    let center = CGPoint(x: frame.midX, y: frame.midY)
    var best: ScreenInfo?
    var bestArea: CGFloat = 0
    for s in screens {
        if s.fullFrame.contains(center) {
            return s
        }
        let inter = s.fullFrame.intersection(frame)
        if !inter.isNull {
            let area = inter.width * inter.height
            if area > bestArea {
                bestArea = area
                best = s
            }
        }
    }
    return best
}

func windowScreenOf(pid: pid_t, stack: [WindowEntry], screens: [ScreenInfo]) -> ScreenInfo? {
    var floating: WindowEntry?
    for e in stack where e.pid == pid {
        if e.isNormal {
            return screenOfFrame(e.frame, screens: screens)
        }
        if floating == nil && e.isFloatingNormalSized {
            floating = e
        }
    }
    if let fl = floating {
        return screenOfFrame(fl.frame, screens: screens)
    }
    return nil
}

func screenContainingMouse(screens: [ScreenInfo]) -> ScreenInfo? {
    let pt = NSEvent.mouseLocation
    return screens.first { $0.nsScreen.frame.contains(pt) }
}

func workingScreen(frontPid: pid_t?, stack: [WindowEntry], screens: [ScreenInfo]) -> ScreenInfo? {
    if let pid = frontPid, let s = windowScreenOf(pid: pid, stack: stack, screens: screens) {
        return s
    }
    if let main = NSScreen.main {
        let num = (main.deviceDescription[NSDeviceDescriptionKey("NSScreenNumber")] as? NSNumber)?.uint32Value ?? 0
        if let found = screens.first(where: { $0.cgid == CGDirectDisplayID(num) }) {
            return found
        }
    }
    return screenContainingMouse(screens: screens) ?? screens.first
}

func findScreen(named name: String, in screens: [ScreenInfo]) -> ScreenInfo? {
    if let exact = screens.first(where: { $0.name == name }) {
        return exact
    }
    let lower = name.lowercased()
    return screens.first(where: { $0.name.lowercased() == lower })
}

func normalWindows(on screen: ScreenInfo, stack: [WindowEntry], screens: [ScreenInfo]) -> [WindowEntry] {
    stack.filter { e in
        e.isNormal && screenOfFrame(e.frame, screens: screens)?.cgid == screen.cgid
    }
}

// MARK: - Space Types (Private SPI with public fallback)

enum SpaceKind {
    case user
    case fullscreen
    case other
}

func currentSpaceInfoByDisplay() -> [String: (id: Int64, kind: SpaceKind)]? {
    let apis = PrivateAPIs.shared
    guard let mc = apis.mainConnectionID, let cs = apis.copyManagedDisplaySpaces else {
        return nil
    }
    let cid = mc()
    guard let arr = cs(cid) as? [[String: Any]] else {
        return nil
    }
    var result: [String: (id: Int64, kind: SpaceKind)] = [:]
    for entry in arr {
        guard let ident = (entry["Display Identifier"] as? String)?.uppercased(),
              let current = entry["Current Space"] as? [String: Any],
              let sid = (current["ManagedSpaceID"] as? NSNumber)?.int64Value
        else { continue }

        let rawType: Int32
        if let st = apis.spaceGetType {
            rawType = st(cid, sid)
        } else if let dt = (current["type"] as? NSNumber)?.int32Value {
            rawType = dt
        } else {
            rawType = -1
        }
        let kind: SpaceKind
        switch rawType {
        case 0: kind = .user
        case 4: kind = .fullscreen
        default: kind = .other
        }
        result[ident] = (sid, kind)
    }
    return result
}

func spaceKind(for screen: ScreenInfo, spaceMap: [String: (id: Int64, kind: SpaceKind)]?, stack: [WindowEntry], screens: [ScreenInfo]) -> (id: Int64?, kind: SpaceKind) {
    if let map = spaceMap {
        if let info = map[screen.uuid] {
            return (info.id, info.kind)
        }
        if screens.count == 1, let mainInfo = map["MAIN"] {
            return (mainInfo.id, mainInfo.kind)
        }
    }
    // Public fallback heuristic: a normal window filling fullFrame is likely fullscreen.
    let norms = normalWindows(on: screen, stack: stack, screens: screens)
    if let top = norms.first {
        if axWindowIsFullscreen(pid: top.pid, cgWindowID: top.id, frame: top.frame) == true {
            return (nil, .fullscreen)
        }
        if abs(top.frame.origin.x - screen.fullFrame.origin.x) <= 2 &&
            abs(top.frame.origin.y - screen.fullFrame.origin.y) <= 2 &&
            abs(top.frame.width - screen.fullFrame.width) <= 2 &&
            abs(top.frame.height - screen.fullFrame.height) <= 2 {
            return (nil, .fullscreen)
        }
    }
    return (nil, .user)
}

// MARK: - Accessibility Helpers

func axWindows(for pid: pid_t) -> [AXUIElement] {
    let appEl = AXUIElementCreateApplication(pid)
    var value: CFTypeRef?
    let err = AXUIElementCopyAttributeValue(appEl, kAXWindowsAttribute as CFString, &value)
    guard err == .success, let arr = value as? [AXUIElement] else { return [] }
    return arr
}

func axStringAttr(_ el: AXUIElement, _ attr: CFString) -> String? {
    var value: CFTypeRef?
    guard AXUIElementCopyAttributeValue(el, attr, &value) == .success else { return nil }
    return value as? String
}

func axBoolAttr(_ el: AXUIElement, _ attr: CFString) -> Bool? {
    var value: CFTypeRef?
    guard AXUIElementCopyAttributeValue(el, attr, &value) == .success else { return nil }
    return (value as? NSNumber)?.boolValue
}

func axFrame(_ el: AXUIElement) -> CGRect? {
    var posRef: CFTypeRef?
    var sizeRef: CFTypeRef?
    guard AXUIElementCopyAttributeValue(el, kAXPositionAttribute as CFString, &posRef) == .success,
          AXUIElementCopyAttributeValue(el, kAXSizeAttribute as CFString, &sizeRef) == .success
    else { return nil }
    var pos = CGPoint.zero
    var size = CGSize.zero
    guard let pRef = posRef, let sRef = sizeRef,
          CFGetTypeID(pRef) == AXValueGetTypeID(),
          CFGetTypeID(sRef) == AXValueGetTypeID(),
          AXValueGetValue((pRef as! AXValue), .cgPoint, &pos),
          AXValueGetValue((sRef as! AXValue), .cgSize, &size)
    else { return nil }
    return CGRect(origin: pos, size: size)
}

func axWindowID(_ el: AXUIElement) -> CGWindowID? {
    guard let fn = PrivateAPIs.shared.axGetWindow else { return nil }
    var wid: CGWindowID = 0
    return fn(el, &wid) == .success ? wid : nil
}

func axFindWindow(pid: pid_t, cgWindowID: CGWindowID?, frame: CGRect?) -> AXUIElement? {
    let wins = axWindows(for: pid)
    if let wid = cgWindowID {
        if let exact = wins.first(where: { axWindowID($0) == wid }) {
            return exact
        }
    }
    if let f = frame {
        if let byFrame = wins.first(where: {
            guard let wf = axFrame($0) else { return false }
            return abs(wf.origin.x - f.origin.x) <= 4 &&
                abs(wf.origin.y - f.origin.y) <= 4 &&
                abs(wf.width - f.width) <= 4 &&
                abs(wf.height - f.height) <= 4
        }) {
            return byFrame
        }
    }
    return nil
}

func axWindowIsFullscreen(pid: pid_t, cgWindowID: CGWindowID, frame: CGRect) -> Bool? {
    guard let w = axFindWindow(pid: pid, cgWindowID: cgWindowID, frame: frame) else { return nil }
    return axBoolAttr(w, "AXFullScreen" as CFString)
}

func axRaiseAndFocus(pid: pid_t, cgWindowID: CGWindowID?, frame: CGRect?) {
    if let w = axFindWindow(pid: pid, cgWindowID: cgWindowID, frame: frame) {
        AXUIElementSetAttributeValue(w, kAXMainAttribute as CFString, kCFBooleanTrue)
        AXUIElementPerformAction(w, kAXRaiseAction as CFString)
    }
}

func axSetFrame(_ el: AXUIElement, _ frame: CGRect) {
    var pos = frame.origin
    var size = frame.size
    if let posVal = AXValueCreate(.cgPoint, &pos) {
        AXUIElementSetAttributeValue(el, kAXPositionAttribute as CFString, posVal)
    }
    if let sizeVal = AXValueCreate(.cgSize, &size) {
        AXUIElementSetAttributeValue(el, kAXSizeAttribute as CFString, sizeVal)
    }
}

func axFindKittyPanelWindow(pid: pid_t, panelEntry: WindowEntry?) -> AXUIElement? {
    let wins = axWindows(for: pid)
    if let pe = panelEntry, let exact = wins.first(where: { axWindowID($0) == pe.id }) {
        return exact
    }
    return wins.first { w in
        let subrole = axStringAttr(w, kAXSubroleAttribute as CFString)
        if subrole == (kAXStandardWindowSubrole as String) { return false }
        if let f = axFrame(w), f.width > kPanelMinDim, f.height > kPanelMinDim {
            return true
        }
        return false
    }
}

// MARK: - Kitty App & Socket

func runningKittyApps() -> [NSRunningApplication] {
    NSRunningApplication.runningApplications(withBundleIdentifier: kittyBundleID)
}

func socketPathForKitty(explicit: String?) -> (app: NSRunningApplication?, socketPath: String?, error: String?) {
    if let exp = explicit, !exp.isEmpty {
        let path = exp.hasPrefix("unix:") ? String(exp.dropFirst(5)) : exp
        let app = runningKittyApps().first
        return FileManager.default.fileExists(atPath: path) ? (app, path, nil) : (app, nil, "socket does not exist: \(path)")
    }

    let apps = runningKittyApps()
    let dir = defaultSocketsDir()
    if apps.isEmpty {
        return (nil, nil, "kitty is not running")
    }

    for app in apps {
        let candidate = "\(dir)/kitty-\(app.processIdentifier).sock"
        var st = stat()
        if stat(candidate, &st) == 0 && (st.st_mode & S_IFMT) == S_IFSOCK {
            return (app, candidate, nil)
        }
    }

    let pid = apps[0].processIdentifier
    return (apps[0], nil, "kitty is running (pid \(pid)) but has no socket in \(dir); restart kitty")
}

func kittyBundleVersion() -> [Int] {
    if let ver = Bundle(path: kittyAppPath)?.infoDictionary?["CFBundleShortVersionString"] as? String {
        let parts = ver.split(separator: ".").prefix(3).compactMap { Int($0) }
        if parts.count >= 3 { return Array(parts) }
    }
    return [0, 48, 2]
}

// MARK: - Kitty Remote Control over Unix Socket

struct KittyRCError: Error, CustomStringConvertible {
    let message: String
    var description: String { message }
}

func kittyRC(socketPath: String, cmd: String, payload: [String: Any], version: [Int]) throws -> Any? {
    let fd = Darwin.socket(AF_UNIX, SOCK_STREAM, 0)
    guard fd >= 0 else {
        throw KittyRCError(message: "socket() failed: \(String(cString: strerror(errno)))")
    }
    defer { Darwin.close(fd) }

    var tv = timeval(tv_sec: 2, tv_usec: 0)
    _ = withUnsafePointer(to: &tv) {
        setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, $0, socklen_t(MemoryLayout<timeval>.size))
    }
    _ = withUnsafePointer(to: &tv) {
        setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, $0, socklen_t(MemoryLayout<timeval>.size))
    }

    var addr = sockaddr_un()
    addr.sun_family = sa_family_t(AF_UNIX)
    let pathBytes = Array(socketPath.utf8CString)
    let maxLen = MemoryLayout.size(ofValue: addr.sun_path)
    guard pathBytes.count <= maxLen else {
        throw KittyRCError(message: "socket path too long: \(socketPath)")
    }
    withUnsafeMutablePointer(to: &addr.sun_path) { ptr in
        ptr.withMemoryRebound(to: CChar.self, capacity: maxLen) { dst in
            for i in 0..<pathBytes.count { dst[i] = pathBytes[i] }
        }
    }

    let connectRes = withUnsafePointer(to: &addr) { ptr in
        ptr.withMemoryRebound(to: sockaddr.self, capacity: 1) { sa in
            Darwin.connect(fd, sa, socklen_t(MemoryLayout<sockaddr_un>.size))
        }
    }
    guard connectRes == 0 else {
        throw KittyRCError(message: "connect(\(socketPath)) failed: \(String(cString: strerror(errno)))")
    }

    let reqObj: [String: Any] = [
        "cmd": cmd,
        "version": version,
        "payload": payload,
    ]
    let jsonData = try JSONSerialization.data(withJSONObject: reqObj, options: [])
    var frame = Data([0x1b, 0x50]) // ESC P
    frame.append(contentsOf: "@kitty-cmd".utf8)
    frame.append(jsonData)
    frame.append(contentsOf: [0x1b, 0x5c]) // ESC \

    try frame.withUnsafeBytes { rawBuf in
        guard let base = rawBuf.baseAddress else { return }
        var sent = 0
        while sent < frame.count {
            let n = Darwin.send(fd, base.advanced(by: sent), frame.count - sent, 0)
            if n <= 0 {
                throw KittyRCError(message: "\(cmd): send failed: \(String(cString: strerror(errno)))")
            }
            sent += n
        }
    }

    var recvData = Data()
    var buf = [UInt8](repeating: 0, count: 8192)
    let endMarker: [UInt8] = [0x1b, 0x5c]
    while true {
        let n = Darwin.recv(fd, &buf, buf.count, 0)
        if n > 0 {
            recvData.append(contentsOf: buf[0..<n])
            if recvData.count >= 2 && recvData.suffix(2).elementsEqual(endMarker) {
                break
            }
        } else if n == 0 {
            break
        } else {
            if errno == EINTR { continue }
            throw KittyRCError(message: "\(cmd): recv failed: \(String(cString: strerror(errno)))")
        }
    }

    let prefixBytes = Array("\u{1b}P@kitty-cmd".utf8)
    guard recvData.count >= prefixBytes.count + 2,
          recvData.prefix(prefixBytes.count).elementsEqual(prefixBytes),
          recvData.suffix(2).elementsEqual(endMarker)
    else {
        throw KittyRCError(message: "\(cmd): unreadable reply")
    }

    let body = recvData.subdata(in: prefixBytes.count..<(recvData.count - 2))
    guard let reply = try JSONSerialization.jsonObject(with: body, options: []) as? [String: Any] else {
        throw KittyRCError(message: "\(cmd): invalid JSON reply")
    }
    guard let ok = reply["ok"] as? Bool, ok else {
        let errStr = reply["error"] as? String ?? "failed"
        throw KittyRCError(message: "\(cmd): \(errStr)")
    }
    return reply["data"]
}

struct PanelState {
    let win: Int?
    let active: Int?
    let firstTab: Int?
    let strays: [Int]
    let output: String?
    let monitors: [String]
    let platformId: CGWindowID?
}

func readPanelState(socketPath: String, stateKittenPath: String, version: [Int]) throws -> PanelState {
    let rawData = try kittyRC(
        socketPath: socketPath,
        cmd: "kitten",
        payload: ["kitten": stateKittenPath, "match": "all"],
        version: version
    )
    guard let jsonStr = rawData as? String,
          let data = jsonStr.data(using: .utf8),
          let dict = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]
    else {
        throw KittyRCError(message: "kitten: unreadable state")
    }
    let strays = (dict["strays"] as? [NSNumber])?.map { $0.intValue } ?? []
    let monitors = dict["monitors"] as? [String] ?? []
    let platformId = (dict["platformId"] as? NSNumber)?.uint32Value
    return PanelState(
        win: (dict["win"] as? NSNumber)?.intValue,
        active: (dict["active"] as? NSNumber)?.intValue,
        firstTab: (dict["firstTab"] as? NSNumber)?.intValue,
        strays: strays,
        output: dict["output"] as? String,
        monitors: monitors,
        platformId: platformId
    )
}

// MARK: - Focus Return & ScreenDecor

struct ReturnTarget {
    let pid: pid_t
    let windowID: CGWindowID
    let frame: CGRect
    let sameScreenFrontmost: Bool
}

func isRegularApp(pid: pid_t) -> Bool {
    guard let app = NSRunningApplication(processIdentifier: pid) else { return false }
    if let bid = app.bundleIdentifier, transientBundleIDs.contains(bid) { return false }
    return app.activationPolicy == .regular
}

func findReturnTarget(on screen: ScreenInfo?, kittyPid: pid_t, stack: [WindowEntry], screens: [ScreenInfo]) -> ReturnTarget? {
    var frontWindowByPid: [pid_t: WindowEntry] = [:]
    var onScreenCandidate: WindowEntry?
    var anywhereCandidate: WindowEntry?

    for e in stack {
        guard e.isNormal, e.pid != kittyPid else { continue }
        if frontWindowByPid[e.pid] == nil {
            frontWindowByPid[e.pid] = e
        }
        guard isRegularApp(pid: e.pid) else { continue }
        if anywhereCandidate == nil {
            anywhereCandidate = e
        }
        if let targetScreen = screen,
           onScreenCandidate == nil,
           screenOfFrame(e.frame, screens: screens)?.cgid == targetScreen.cgid {
            onScreenCandidate = e
            break
        }
    }

    guard let chosen = onScreenCandidate ?? anywhereCandidate else { return nil }
    let frontSame = frontWindowByPid[chosen.pid]?.id == chosen.id
    return ReturnTarget(pid: chosen.pid, windowID: chosen.id, frame: chosen.frame, sameScreenFrontmost: frontSame)
}

func activateReturnTarget(_ target: ReturnTarget) {
    guard let app = NSRunningApplication(processIdentifier: target.pid) else { return }
    activateApp(app)
    if !target.sameScreenFrontmost {
        axRaiseAndFocus(pid: target.pid, cgWindowID: target.windowID, frame: target.frame)
    }
}

func ensureScreenDecorBuilt(appPath: String, buildScript: String) -> Bool {
    let binPath = appPath + "/Contents/MacOS/ScreenDecor"
    let fm = FileManager.default
    if fm.isExecutableFile(atPath: binPath) {
        return true
    }
    guard fm.isExecutableFile(atPath: buildScript) else {
        return false
    }
    let proc = Process()
    proc.executableURL = URL(fileURLWithPath: buildScript)
    proc.arguments = [appPath]
    proc.standardOutput = FileHandle.nullDevice
    proc.standardError = FileHandle.nullDevice
    do {
        try proc.run()
        proc.waitUntilExit()
        return proc.terminationStatus == 0 && fm.isExecutableFile(atPath: binPath)
    } catch {
        return false
    }
}

func prepareScreenDecor(
    targetScreen: ScreenInfo,
    kittyPid: pid_t?,
    stack: [WindowEntry],
    screens: [ScreenInfo],
    decorAppPath: String,
    decorBuildScript: String
) {
    guard screens.count > 1 else { return }
    let spaceMap = currentSpaceInfoByDisplay()
    let targetSpace = spaceKind(for: targetScreen, spaceMap: spaceMap, stack: stack, screens: screens)
    guard targetSpace.kind == .fullscreen else { return }

    var toCover: [ScreenInfo] = []
    for s in screens where s.cgid != targetScreen.cgid {
        let sk = spaceKind(for: s, spaceMap: spaceMap, stack: stack, screens: screens)
        if sk.kind == .user && normalWindows(on: s, stack: stack, screens: screens).isEmpty {
            toCover.append(s)
        }
    }
    guard !toCover.isEmpty else { return }
    guard ensureScreenDecorBuilt(appPath: decorAppPath, buildScript: decorBuildScript) else { return }

    // Remember who currently has focus on targetScreen so we can restore it before showing kitty.
    let frontApp = NSWorkspace.shared.frontmostApplication
    var backPid: pid_t?
    var backWin: WindowEntry?
    if let fp = frontApp?.processIdentifier,
       fp != kittyPid,
       windowScreenOf(pid: fp, stack: stack, screens: screens)?.cgid == targetScreen.cgid,
       isRegularApp(pid: fp) {
        backPid = fp
        backWin = stack.first(where: { $0.pid == fp && $0.isNormal })
    } else if let top = normalWindows(on: targetScreen, stack: stack, screens: screens).first(where: { $0.pid != kittyPid && isRegularApp(pid: $0.pid) }) {
        backPid = top.pid
        backWin = top
    }

    let runningDecor = NSRunningApplication.runningApplications(withBundleIdentifier: screenDecorBundleID)
    let waitSeconds: TimeInterval
    if !runningDecor.isEmpty {
        let dnc = DistributedNotificationCenter.default()
        for s in toCover where !s.uuid.isEmpty {
            dnc.postNotificationName(screenDecorCoverNote, object: s.uuid, userInfo: nil, deliverImmediately: true)
        }
        waitSeconds = 1.5
    } else {
        var args = ["-a", decorAppPath, "--args"]
        for s in toCover where !s.uuid.isEmpty {
            args.append("--cover")
            args.append(s.uuid)
        }
        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: "/usr/bin/open")
        proc.arguments = args
        proc.standardOutput = FileHandle.nullDevice
        proc.standardError = FileHandle.nullDevice
        try? proc.run()
        waitSeconds = 4.0
    }

    let deadline = Date().addingTimeInterval(waitSeconds)
    while Date() < deadline {
        let curMap = currentSpaceInfoByDisplay()
        let curStack = readWindowStack()
        let allFullscreen = toCover.allSatisfy { s in
            spaceKind(for: s, spaceMap: curMap, stack: curStack, screens: screens).kind == .fullscreen
        }
        if allFullscreen { break }
        Thread.sleep(forTimeInterval: 0.025)
    }

    if let bp = backPid, let bApp = NSRunningApplication(processIdentifier: bp) {
        activateApp(bApp)
        if let bw = backWin {
            axRaiseAndFocus(pid: bp, cgWindowID: bw.id, frame: bw.frame)
        }
        let refocusDeadline = Date().addingTimeInterval(0.6)
        while Date() < refocusDeadline {
            if NSWorkspace.shared.frontmostApplication?.processIdentifier == bp {
                break
            }
            Thread.sleep(forTimeInterval: 0.02)
        }
    }
}

// MARK: - Post-Show Frame Fit & Space Repair

func frameNear(_ a: CGRect, _ b: CGRect) -> Bool {
    abs(a.origin.x - b.origin.x) <= kPanelFitSlack &&
        abs(a.origin.y - b.origin.y) <= kPanelFitSlack &&
        abs(a.width - b.width) <= kPanelFitSlack &&
        abs(a.height - b.height) <= kPanelFitSlack
}

func findKittyPanelEntry(kittyPid: pid_t, stack: [WindowEntry]) -> WindowEntry? {
    stack.first { $0.pid == kittyPid && $0.isPanelSized }
}

func fitPanelFrame(kittyPid: pid_t, targetScreen: ScreenInfo, quiet: Bool) -> Bool {
    var entry: WindowEntry?
    let deadline = Date().addingTimeInterval(0.18)
    while Date() < deadline {
        entry = findKittyPanelEntry(kittyPid: kittyPid, stack: readWindowStack())
        if entry != nil { break }
        Thread.sleep(forTimeInterval: 0.025)
    }

    guard let panel = entry else {
        return false
    }

    let want = targetScreen.expectedPanelFrame
    if frameNear(panel.frame, want) {
        return true
    }

    guard let axWin = axFindKittyPanelWindow(pid: kittyPid, panelEntry: panel) else {
        if !quiet { logErr("fit: could not locate panel AXWindow") }
        return true
    }
    axSetFrame(axWin, want)
    return true
}

func repairPanelSpaceIfNeeded(
    socketPath: String,
    state: PanelState,
    kittyPid: pid_t,
    targetScreen: ScreenInfo,
    version: [Int],
    quiet: Bool
) {
    if fitPanelFrame(kittyPid: kittyPid, targetScreen: targetScreen, quiet: quiet) {
        return
    }
    // Panel landed off-screen (e.g. in another monitor's desktop Space behind a fullscreen Space).
    let stack = readWindowStack()
    let screens = readScreens()
    let spaceMap = currentSpaceInfoByDisplay()
    let targetSpace = spaceKind(for: targetScreen, spaceMap: spaceMap, stack: stack, screens: screens)

    if targetSpace.kind == .user,
       let sid = targetSpace.id,
       let wid = state.platformId,
       let mc = PrivateAPIs.shared.mainConnectionID,
       let moveFn = PrivateAPIs.shared.moveWindowsToManagedSpace {
        let ids: CFArray = [NSNumber(value: wid)] as CFArray
        moveFn(mc(), ids, sid)
        _ = fitPanelFrame(kittyPid: kittyPid, targetScreen: targetScreen, quiet: quiet)
        return
    }

    // Fullscreen target Space: focus a window on targetScreen so CGSGetActiveSpace() matches it, then re-show once.
    if let frontWin = normalWindows(on: targetScreen, stack: stack, screens: screens).first,
       let app = NSRunningApplication(processIdentifier: frontWin.pid) {
        activateApp(app)
        axRaiseAndFocus(pid: frontWin.pid, cgWindowID: frontWin.id, frame: frontWin.frame)
        Thread.sleep(forTimeInterval: 0.08)
    }

    guard let winID = state.win else { return }
    let matchWin = "id:\(winID)"
    _ = try? kittyRC(socketPath: socketPath, cmd: "resize-os-window", payload: ["match": matchWin, "action": "hide"], version: version)
    _ = try? kittyRC(socketPath: socketPath, cmd: "resize-os-window", payload: ["match": matchWin, "action": "os-panel", "incremental": true, "os_panel": ["output-name=\(targetScreen.name)"]], version: version)
    _ = try? kittyRC(socketPath: socketPath, cmd: "resize-os-window", payload: ["match": matchWin, "action": "show"], version: version)
    if let activeID = state.active {
        _ = try? kittyRC(socketPath: socketPath, cmd: "focus-window", payload: ["match": "id:\(activeID)"], version: version)
    }
    _ = fitPanelFrame(kittyPid: kittyPid, targetScreen: targetScreen, quiet: quiet)
}

// MARK: - CLI Options & Actions

struct Options {
    var action: String = "toggle"
    var screenName: String?
    var socketPath: String?
    var foldStrays: Bool = false
    var useDecor: Bool = true
    var useFit: Bool = true
    var quiet: Bool = false
    var stateKittenPath: String = defaultNightDir() + "/configFiles/kitty/kitty_panel_state.py"
    var decorAppPath: String = NSHomeDirectory() + "/Applications/ScreenDecor.app"
    var decorBuildScript: String = defaultNightDir() + "/hammerspoon/screen-decor/build.sh"
}

func parseArgs() -> Options {
    var opts = Options()
    var it = CommandLine.arguments.dropFirst().makeIterator()
    while let arg = it.next() {
        switch arg {
        case "toggle", "show", "hide", "screen", "decor-prepare", "fit", "panel-shown", "focus-back":
            opts.action = arg
        case "--screen":
            opts.screenName = it.next()
        case "--socket":
            opts.socketPath = it.next()
        case "--fold-strays":
            opts.foldStrays = true
        case "--no-decor":
            opts.useDecor = false
        case "--no-fit":
            opts.useFit = false
        case "--quiet", "-q":
            opts.quiet = true
        case "--state-kitten":
            if let v = it.next() { opts.stateKittenPath = v }
        case "--decor-app":
            if let v = it.next() { opts.decorAppPath = v }
        case "--decor-build":
            if let v = it.next() { opts.decorBuildScript = v }
        case "-h", "--help":
            print("Usage: kitty_panel.swift [toggle|show|hide|screen|decor-prepare|fit|panel-shown|focus-back] [options]")
            exit(0)
        default:
            logErr("unknown argument: \(arg)")
            exit(2)
        }
    }
    return opts
}

func run() -> Int32 {
    let opts = parseArgs()
    let screens = readScreens()
    let stack = readWindowStack()
    let frontApp = NSWorkspace.shared.frontmostApplication
    let frontPid = frontApp?.processIdentifier
    let (kittyApp, sockPath, sockErr) = socketPathForKitty(explicit: opts.socketPath)
    let kittyPid = kittyApp?.processIdentifier
    let kittyFrontmost = (kittyPid != nil && frontPid == kittyPid)
    let panelEntry = kittyPid.flatMap { findKittyPanelEntry(kittyPid: $0, stack: stack) }
    let panelShown = kittyFrontmost && (panelEntry != nil)

    func resolveTargetScreen() -> ScreenInfo? {
        if let name = opts.screenName, !name.isEmpty {
            if let found = findScreen(named: name, in: screens) { return found }
        }
        return workingScreen(frontPid: frontPid, stack: stack, screens: screens)
    }

    switch opts.action {
    case "screen":
        if let s = resolveTargetScreen() {
            print(s.name)
            return 0
        }
        return 1

    case "panel-shown":
        return panelShown ? 0 : 1

    case "decor-prepare":
        if opts.useDecor, let s = resolveTargetScreen() {
            prepareScreenDecor(
                targetScreen: s,
                kittyPid: kittyPid,
                stack: stack,
                screens: screens,
                decorAppPath: opts.decorAppPath,
                decorBuildScript: opts.decorBuildScript
            )
        }
        return 0

    case "fit":
        guard let kp = kittyPid, let s = resolveTargetScreen() else { return 0 }
        _ = fitPanelFrame(kittyPid: kp, targetScreen: s, quiet: opts.quiet)
        return 0

    case "focus-back":
        guard let kp = kittyPid else { return 0 }
        let pScreen = panelEntry.flatMap { screenOfFrame($0.frame, screens: screens) }
            ?? resolveTargetScreen()
        if let target = findReturnTarget(on: pScreen, kittyPid: kp, stack: stack, screens: screens) {
            activateReturnTarget(target)
        }
        return 0

    case "toggle", "show", "hide":
        let wantShow: Bool
        switch opts.action {
        case "show": wantShow = true
        case "hide": wantShow = false
        default: wantShow = !panelShown
        }

        if !wantShow {
            // HIDE
            guard let kp = kittyPid else { return 0 }
            guard let sock = sockPath else {
                if let err = sockErr, !opts.quiet { logErr(err) }
                return 0
            }
            let pScreen = panelEntry.flatMap { screenOfFrame($0.frame, screens: screens) }
                ?? resolveTargetScreen()
            let returnTarget = kittyFrontmost ? findReturnTarget(on: pScreen, kittyPid: kp, stack: stack, screens: screens) : nil

            let version = kittyBundleVersion()
            do {
                let st = try readPanelState(socketPath: sock, stateKittenPath: opts.stateKittenPath, version: version)
                guard let winID = st.win else { return 0 }
                _ = try kittyRC(socketPath: sock, cmd: "resize-os-window", payload: ["match": "id:\(winID)", "action": "hide"], version: version)
                if let target = returnTarget {
                    activateReturnTarget(target)
                }
                return 0
            } catch {
                if !opts.quiet { logErr("fast hide failed (\(error)); falling back to kitten") }
                return exitNeedsSlowHide
            }
        } else {
            // SHOW
            guard let kp = kittyPid else {
                return exitNeedsLaunch
            }
            guard let sock = sockPath else {
                if let err = sockErr { logErr(err) }
                return 1
            }

            let version = kittyBundleVersion()
            let state: PanelState
            do {
                state = try readPanelState(socketPath: sock, stateKittenPath: opts.stateKittenPath, version: version)
            } catch {
                if !opts.quiet { logErr("fast state failed (\(error)); falling back to kitten") }
                return exitNeedsSlowShow
            }

            guard let winID = state.win else {
                return exitNeedsSlowShow
            }
            if opts.foldStrays && !state.strays.isEmpty {
                return exitNeedsSlowShow
            }

            let targetScreen = resolveTargetScreen()
            if opts.useDecor, let ts = targetScreen {
                prepareScreenDecor(
                    targetScreen: ts,
                    kittyPid: kp,
                    stack: stack,
                    screens: screens,
                    decorAppPath: opts.decorAppPath,
                    decorBuildScript: opts.decorBuildScript
                )
            }

            let matchWin = "id:\(winID)"
            _ = try? kittyRC(socketPath: sock, cmd: "resize-os-window", payload: ["match": matchWin, "action": "hide"], version: version)

            if let ts = targetScreen {
                do {
                    _ = try kittyRC(
                        socketPath: sock,
                        cmd: "resize-os-window",
                        payload: [
                            "match": matchWin,
                            "action": "os-panel",
                            "incremental": true,
                            "os_panel": ["output-name=\(ts.name)"],
                        ],
                        version: version
                    )
                } catch {
                    if !opts.quiet { logErr("move to \(ts.name) failed: \(error)") }
                }
            }

            do {
                _ = try kittyRC(socketPath: sock, cmd: "resize-os-window", payload: ["match": matchWin, "action": "show"], version: version)
                if let activeID = state.active {
                    _ = try kittyRC(socketPath: sock, cmd: "focus-window", payload: ["match": "id:\(activeID)"], version: version)
                }
            } catch {
                if !opts.quiet { logErr("fast show failed (\(error)); falling back to kitten") }
                return exitNeedsSlowShow
            }

            if opts.useFit, let ts = targetScreen {
                repairPanelSpaceIfNeeded(
                    socketPath: sock,
                    state: state,
                    kittyPid: kp,
                    targetScreen: ts,
                    version: version,
                    quiet: opts.quiet
                )
            }
            return 0
        }

    default:
        return 2
    }
}

exit(run())
