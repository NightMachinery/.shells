#!/usr/bin/env scriptisto

// scriptisto-begin
// script_src: main.swift
// build_cmd: swiftc main.swift -o ./script
// scriptisto-end

// Reads and writes the mute flag of a SPECIFIC audio output device.
//
// macOS mute is per-device and persistent, and `osascript ... output muted of
// (get volume settings)' only ever reports the current DEFAULT output device.
// So it cannot say whether the laptop speakers are muted while you are on
// headphones -- which is the state this exists to report.
//
// system_profiler SPAudioDataType cannot answer it either: it reports name,
// manufacturer, transport and which device is default, and no mute field at
// all. Hence CoreAudio, and hence this file. See ../docs/audio-device-mute.md.
//
// Usage:
//   audio_device_mute.swift get <device>
//   audio_device_mute.swift set <device> <true|false>
//   audio_device_mute.swift list
//
// <device> is `builtin', a device UID, or an exact device name.
// `get'/`set' print `true' or `false'; `list' prints
// uid<TAB>name<TAB>transport<TAB>muted for every output device.
//
// Exit codes: 0 ok, 3 no such device, 4 device has no mute control, 1 other.
// The zsh side maps 3 and 4 onto its own "unknown" status; see
// [agfi:volume-mute-device-p].

import CoreAudio
import Foundation

let EXIT_NODEVICE: Int32 = 3
let EXIT_NOMUTE: Int32 = 4

func die(_ msg: String, _ code: Int32 = 1) -> Never {
    FileHandle.standardError.write((msg + "\n").data(using: .utf8)!)
    exit(code)
}

//: ** CoreAudio property helpers

func address(
    _ selector: AudioObjectPropertySelector,
    _ scope: AudioObjectPropertyScope = kAudioObjectPropertyScopeGlobal,
    _ element: AudioObjectPropertyElement = kAudioObjectPropertyElementMain
) -> AudioObjectPropertyAddress {
    AudioObjectPropertyAddress(mSelector: selector, mScope: scope, mElement: element)
}

func fixedProperty<T>(_ object: AudioObjectID, _ addr: AudioObjectPropertyAddress, _ fallback: T) -> T? {
    var addr = addr
    var value = fallback
    var size = UInt32(MemoryLayout<T>.size)
    let status = AudioObjectGetPropertyData(object, &addr, 0, nil, &size, &value)
    return status == noErr ? value : nil
}

func stringProperty(_ object: AudioObjectID, _ addr: AudioObjectPropertyAddress) -> String? {
    var addr = addr
    var value: CFString = "" as CFString
    var size = UInt32(MemoryLayout<CFString>.size)
    let status = AudioObjectGetPropertyData(object, &addr, 0, nil, &size, &value)
    return status == noErr ? (value as String) : nil
}

//: ** Devices

struct Device {
    let id: AudioObjectID
    let uid: String
    let name: String
    let transport: String
}

//: A four-character code, the way CoreAudio spells transports. Mapped to the
//: same words `hs.audiodevice:transportType()' uses, so the two backends of
//: [agfi:volume-mute-device-p] cannot disagree about what a device is.
func transportName(_ raw: UInt32) -> String {
    switch raw {
    case kAudioDeviceTransportTypeBuiltIn: return "Built-in"
    case kAudioDeviceTransportTypeBluetooth,
         kAudioDeviceTransportTypeBluetoothLE: return "Bluetooth"
    case kAudioDeviceTransportTypeUSB: return "USB"
    case kAudioDeviceTransportTypeDisplayPort: return "DisplayPort"
    case kAudioDeviceTransportTypeHDMI: return "HDMI"
    case kAudioDeviceTransportTypeAirPlay: return "AirPlay"
    case kAudioDeviceTransportTypeVirtual: return "Virtual"
    case kAudioDeviceTransportTypeAggregate: return "Aggregate"
    case kAudioDeviceTransportTypeThunderbolt: return "Thunderbolt"
    case kAudioDeviceTransportTypeFireWire: return "FireWire"
    case kAudioDeviceTransportTypePCI: return "PCI"
    case kAudioDeviceTransportTypeAVB: return "AVB"
    case kAudioDeviceTransportTypeContinuityCaptureWired,
         kAudioDeviceTransportTypeContinuityCapture: return "Continuity"
    default: return "Unknown"
    }
}

//: Output devices only. Without this the built-in MICROPHONE also matches
//: `builtin', and it is a different device with its own mute flag.
func hasOutputStreams(_ id: AudioObjectID) -> Bool {
    var addr = address(kAudioDevicePropertyStreamConfiguration, kAudioObjectPropertyScopeOutput)
    var size: UInt32 = 0
    guard AudioObjectGetPropertyDataSize(id, &addr, 0, nil, &size) == noErr, size > 0 else {
        return false
    }

    let raw = UnsafeMutableRawPointer.allocate(byteCount: Int(size), alignment: MemoryLayout<AudioBufferList>.alignment)
    defer { raw.deallocate() }
    guard AudioObjectGetPropertyData(id, &addr, 0, nil, &size, raw) == noErr else { return false }

    let lists = UnsafeMutableAudioBufferListPointer(raw.assumingMemoryBound(to: AudioBufferList.self))
    return lists.contains { $0.mNumberChannels > 0 }
}

func outputDevices() -> [Device] {
    var addr = address(kAudioHardwarePropertyDevices)
    var size: UInt32 = 0
    guard AudioObjectGetPropertyDataSize(AudioObjectID(kAudioObjectSystemObject), &addr, 0, nil, &size) == noErr else {
        die("audio_device_mute: could not enumerate audio devices.")
    }

    var ids = [AudioObjectID](repeating: 0, count: Int(size) / MemoryLayout<AudioObjectID>.size)
    guard AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &addr, 0, nil, &size, &ids) == noErr else {
        die("audio_device_mute: could not read the audio device list.")
    }

    return ids.filter(hasOutputStreams).map { id in
        Device(
            id: id,
            uid: stringProperty(id, address(kAudioDevicePropertyDeviceUID)) ?? "",
            name: stringProperty(id, address(kAudioObjectPropertyName)) ?? "",
            transport: transportName(fixedProperty(id, address(kAudioDevicePropertyTransportType), UInt32(0)) ?? 0)
        )
    }
}

//: `builtin' first by UID, then by transport. The UID is stable across Apple
//: Silicon Macs and the transport scan is the safety net; the NAME is model
//: dependent ("MacBook Air Speakers", "MacBook Pro Speakers", ...) and is
//: deliberately never matched against here.
let builtinSpeakerUID = "BuiltInSpeakerDevice"

func findDevice(_ spec: String, _ devices: [Device]) -> Device? {
    if spec == "builtin" {
        return devices.first { $0.uid == builtinSpeakerUID }
            ?? devices.first { $0.transport == "Built-in" }
    }

    return devices.first { $0.uid == spec } ?? devices.first { $0.name == spec }
}

//: ** Mute

//: The main element carries the master mute on every device that has one, but
//: some expose it per channel only, so fall back to the first two channels
//: rather than reporting "no mute control" for a device that has one.
let muteElements: [AudioObjectPropertyElement] = [kAudioObjectPropertyElementMain, 1, 2]

func muteAddress(_ device: Device, settable: Bool) -> AudioObjectPropertyAddress? {
    for element in muteElements {
        var addr = address(kAudioDevicePropertyMute, kAudioObjectPropertyScopeOutput, element)
        guard AudioObjectHasProperty(device.id, &addr) else { continue }

        if settable {
            var isSettable: DarwinBoolean = false
            guard AudioObjectIsPropertySettable(device.id, &addr, &isSettable) == noErr, isSettable.boolValue else {
                continue
            }
        }
        return addr
    }
    return nil
}

func muteGet(_ device: Device) -> Bool? {
    guard let addr = muteAddress(device, settable: false) else { return nil }
    guard let value: UInt32 = fixedProperty(device.id, addr, UInt32(0)) else { return nil }
    return value != 0
}

func muteSet(_ device: Device, _ muted: Bool) -> Bool {
    guard var addr = muteAddress(device, settable: true) else { return false }
    var value: UInt32 = muted ? 1 : 0
    let status = AudioObjectSetPropertyData(device.id, &addr, 0, nil, UInt32(MemoryLayout<UInt32>.size), &value)
    return status == noErr
}

//: ** Entry point

let args = Array(CommandLine.arguments.dropFirst())
let usage = """
Usage:
  audio_device_mute.swift get <device>
  audio_device_mute.swift set <device> <true|false>
  audio_device_mute.swift list

<device> is `builtin', a device UID, or an exact device name.
"""

guard let command = args.first else { die(usage) }

let devices = outputDevices()

switch command {
case "list":
    for d in devices {
        let muted = muteGet(d).map(String.init(describing:)) ?? "nomute"
        print("\(d.uid)\t\(d.name)\t\(d.transport)\t\(muted)")
    }

case "get":
    guard args.count == 2 else { die(usage) }
    guard let device = findDevice(args[1], devices) else {
        die("audio_device_mute: no such output device: \(args[1])", EXIT_NODEVICE)
    }
    guard let muted = muteGet(device) else {
        die("audio_device_mute: \(device.name) has no mute control.", EXIT_NOMUTE)
    }
    print(muted)

case "set":
    guard args.count == 3, let wanted = Bool(args[2]) else { die(usage) }
    guard let device = findDevice(args[1], devices) else {
        die("audio_device_mute: no such output device: \(args[1])", EXIT_NODEVICE)
    }
    guard muteSet(device, wanted) else {
        die("audio_device_mute: \(device.name) has no writable mute control.", EXIT_NOMUTE)
    }
    //: Print the state AFTER the write, not the state we asked for: a device
    //: that accepts the request and ignores it is the failure worth catching.
    guard let muted = muteGet(device) else {
        die("audio_device_mute: \(device.name) has no readable mute control.", EXIT_NOMUTE)
    }
    print(muted)

default:
    die(usage)
}
