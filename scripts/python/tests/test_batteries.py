"""Synthetic battery fixtures: no live device inventory or identifying details."""
import argparse
from datetime import datetime, timedelta, timezone
import importlib.util
import io
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from unittest.mock import patch

SPEC = importlib.util.spec_from_file_location("batteries", Path(__file__).parents[1] / "batteries.py")
b = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(b)


def inventory(*devices):
    return {"SPBluetoothDataType": [{"device_connected": [{name: properties} for name, properties in devices],
                                    "device_not_connected": [{"Old mouse": {"device_batteryLevelMain": "90%"}}]}]}


def host(name, *rows):
    return dict(id=name, name=name, local=False, platform="macOS", online=True, status="ok",
                observed_at=b.now(), batteries=list(rows), warnings=[])


class BatteriesTest(unittest.TestCase):
    def test_laptop_state_and_unknown_are_not_zero(self):
        rows = b.laptop_readings(" -InternalBattery-0\t78%; AC attached; not charging present: true")
        self.assertEqual(rows[0]["level_percent"], 78)
        self.assertEqual(rows[0]["charging"], "no")
        self.assertEqual(rows[0]["detail"], "AC attached; not charging")
        self.assertEqual(b.laptop_readings("12%; charging; 1:20 remaining present: true")[0]["charging"], "yes")
        for value in (None, True, -1, 101, "unknown", ""):
            self.assertIsNone(b.percentage(value))
        self.assertEqual(b.percentage(0), 0)

    def test_hid_renamed_device_and_duplicate_service(self):
        device = dict(Product="Magic Keyboard", DeviceAddress="00-00-00-00-00-01", BatteryPercent=73, Transport="Bluetooth")
        rows = b.mac_peripherals(inventory(("Desk keyboard", {"device_address": "00:00:00:00:00:01"})), [device, device], {})
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["name"], "Desk keyboard")
        self.assertEqual(rows[0]["level_percent"], 73)

    def test_disconnected_omitted_usb_zero_and_inventory_failure(self):
        registry = [dict(Product="Old mouse", BatteryPercent=90, Transport="Bluetooth"),
                    dict(Product="USB mouse", BatteryPercent=0, Transport="USB")]
        rows = b.mac_peripherals(inventory(), registry, {})
        self.assertEqual([(row["name"], row["level_percent"]) for row in rows], [("USB mouse", 0)])
        self.assertEqual(len(b.mac_peripherals(None, registry, {})), 2)

    def test_earbuds_parts_and_charging(self):
        properties = dict(device_address="00:00:00:00:00:02", device_vendorID="0x054C")
        parts = {"000000000002": {"batteries": [
            dict(part="left", level_percent=0, charging="no"),
            dict(part="case", level_percent=None, charging="unknown"),
            dict(part="right", level_percent=80, charging="yes")]}}
        rows = b.mac_peripherals(inventory(("Renamed headset", properties)), [], parts)
        self.assertEqual(len(rows), 3)
        self.assertEqual({r["part"]: r["level_percent"] for r in rows}, {"left": 0, "case": None, "right": 80})
        self.assertEqual(rows[2]["charging"], "yes")
        self.assertTrue(all(r["identity"] == "bluetooth:000000000002" for r in rows))

    def test_generic_earbuds(self):
        rows = b.mac_peripherals(inventory(("Buds", dict(device_batteryLevelLeft="10%", device_batteryLevelCase=None))), [], {})
        self.assertEqual({r["part"]: r["level_percent"] for r in rows}, {"left": 10, "case": None})

    def test_headphone_queries_address_each_connected_device_and_skip_disconnected(self):
        devices = inventory(("Bose one", dict(device_address="00:00:00:00:00:01")),
                            ("Bose two", dict(device_address="00:00:00:00:00:02")))
        calls = []

        class FixtureReader:
            warnings = []

            def get(self, label, argv, parse=None):
                calls.append(argv)
                if "system_profiler" in argv[0]:
                    return devices
                if "ioreg" in argv[0]:
                    return []
                if "pmset" in argv[0]:
                    return "50%; discharging present: true"
                return {"batteries": [dict(part="battery", level_percent=42, charging="no")]}

        with patch.object(b.shutil, "which", return_value="/synthetic/bosectl"):
            rows = b.probe_mac(FixtureReader())
        queries = [argv for argv in calls if argv[0] == "/synthetic/bosectl"]
        self.assertEqual(len(queries), 2)
        self.assertEqual({argv[argv.index("--mac")+1] for argv in queries},
                         {"00:00:00:00:00:01", "00:00:00:00:00:02"})
        self.assertTrue(all("--auto-connect" not in argv for argv in queries))
        self.assertEqual(len(rows), 3)

    def test_multipoint_one_identity_two_hosts_and_conflicting_readings(self):
        left = b.reading("Headset", 70, identity="bluetooth:000000000003", charging="no")
        right = b.reading("Other name", 80, identity="bluetooth:000000000003", charging="yes")
        merged = b.merge_devices([host("laptop-a", left), host("laptop-b", right)])
        self.assertEqual(len(merged), 1)
        row = merged[0]
        self.assertEqual(row["hosts"], ["laptop-a", "laptop-b"])
        self.assertTrue(row["conflict"])
        self.assertTrue(row["charging_conflict"])
        self.assertIsNone(row["level_percent"])
        self.assertEqual((row["level_min"], row["level_max"]), (70, 80))
        self.assertEqual(row["names"], ["Headset", "Other name"])

    def test_same_names_do_not_merge_without_same_address(self):
        rows = [host("a", b.reading("Mouse", 5, identity="bluetooth:000000000001"), b.reading("USB mouse", 50, identity="local:usb")),
                host("b", b.reading("Mouse", 5, identity="bluetooth:000000000002"), b.reading("USB mouse", 50, identity="local:usb"))]
        self.assertEqual(len(b.merge_devices(rows)), 4)

    def test_known_and_unknown_multipoint_readings_preserve_known_value(self):
        a = b.reading("Headset", None, identity="bluetooth:000000000003")
        c = b.reading("Headset", 0, identity="bluetooth:000000000003")
        row = b.merge_devices([host("a", a), host("b", c)])[0]
        self.assertEqual(row["level_percent"], 0)
        self.assertFalse(row["conflict"])

    def test_bluez_only_connected_and_known_zero(self):
        listing = "Device 00:00:00:00:00:01 Mouse\nDevice 00:00:00:00:00:02 Headset"
        rows = b.bluez_readings(listing, {"00:00:00:00:00:01": "Connected: yes\nBattery Percentage: 0x00 (0)",
                                        "00:00:00:00:00:02": "Connected: no\nBattery Percentage: 0x64 (100)"})
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["level_percent"], 0)

    def test_query_remote_streams_source_and_never_installs(self):
        peer = dict(id="laptop-b", name="laptop-b", target="work-laptop", platform="macOS", local=False, online=True)
        with patch.object(b.subprocess, "run", return_value=subprocess.CompletedProcess([], 0, '{"batteries":[],"status":"ok"}', '')) as run:
            result = b.query_host(peer, "# collector source", 18)
        self.assertEqual(result["status"], "ok")
        argv = run.call_args.args[0]
        self.assertEqual(argv[-2], "work-laptop")
        self.assertIn("python3 - --probe", argv[-1])
        self.assertEqual(run.call_args.kwargs["input"], "# collector source")
        self.assertEqual(run.call_args.kwargs["timeout"], 18)

    def test_offline_phone_does_not_start_ssh(self):
        peer = dict(id="phone", name="phone", target="phone", platform="iOS", local=False, online=False)
        with patch.object(b.subprocess, "run") as run:
            result = b.query_host(peer, "source", 5)
        run.assert_not_called()
        self.assertEqual(result["status"], "offline")

    def test_ssh_alias_from_included_config_and_peer_dns(self):
        with tempfile.TemporaryDirectory() as folder:
            root = Path(folder)
            (root / "included").write_text("Host phone-alias\n HostName phone.example.ts.net\n Port 8022\n")
            (root / "config").write_text(f"Include {root}/included\nHost notebook\n HostName laptop.example.ts.net.\nHost *\n User ignored\n")
            mapping = b.ssh_aliases(root / "config")
            peer = b.peer_record(dict(DNSName="phone.example.ts.net.", HostName="Phone", OS="android", Online=True), aliases=mapping)
            self.assertEqual(peer["target"], "phone-alias")
            self.assertEqual(mapping["laptop.example.ts.net"], "notebook")

    def test_phone_upload_cache_atomic_and_age_visible(self):
        with tempfile.TemporaryDirectory() as folder, patch.object(b, "cache_directory", return_value=Path(folder)):
            record = b.receive("phone", io.StringIO('{"level_percent":76,"charging":"no"}'))
            self.assertEqual(len(list(Path(folder).iterdir())), 1)
            record["received_at"] = (datetime.now(timezone.utc) - timedelta(hours=2)).isoformat()
            (Path(folder) / "phone.json").write_text(json.dumps(record))
            hosts = [dict(id="phone.example", name="phone", status="offline", batteries=[])]
            b.apply_published(hosts, 3600)
            self.assertEqual(hosts[0]["status"], "cached")
            self.assertTrue(hosts[0]["stale"])
            self.assertEqual(hosts[0]["batteries"][0]["level_percent"], 76)
        for name, payload in (("../phone", '{}'), ("phone", '{"level_percent":101}')):
            with self.assertRaises(ValueError):
                b.receive(name, io.StringIO(payload))

    def test_deadline_and_failed_source_report_unknown(self):
        reader = b.Reader(5)
        with patch.object(b.subprocess, "run", side_effect=subprocess.TimeoutExpired("ioreg", 4)) as run:
            self.assertIsNone(reader.get("HID", ["ioreg"]))
        self.assertEqual(reader.warnings, ["HID unavailable"])
        self.assertLessEqual(run.call_args.kwargs["timeout"], 4)

    def test_uploaded_phone_reading_can_arrive_via_another_laptop(self):
        published = dict(host="phone", received_at=b.now(), battery=b.reading("Phone", 76, kind="phone"))
        hosts = [dict(id="laptop", name="laptop", batteries=[], published=[published]),
                 dict(id="phone.example", name="phone", batteries=[], status="unsupported")]
        with patch.object(b, "published_snapshots", return_value=[]):
            b.apply_published(hosts, 3600)
        self.assertEqual(hosts[1]["status"], "cached")
        self.assertFalse(hosts[1]["stale"])
        self.assertEqual(hosts[1]["batteries"][0]["level_percent"], 76)

    def test_local_scope_does_not_discover_and_json_is_parseable(self):
        with patch.object(b, "discover") as discover, patch.object(b, "probe", return_value=dict(status="ok", batteries=[], published=[])), \
             patch.object(b, "published_snapshots", return_value=[]), patch.object(sys, "argv", ["batteries", "--local", "--json"]):
            out = io.StringIO()
            with redirect_stdout(out):
                self.assertEqual(b.main(), 0)
        discover.assert_not_called()
        self.assertEqual(json.loads(out.getvalue())["schema_version"], 1)
        self.assertNotIn("\033", out.getvalue())

    def test_render_deduplicated_multipoint_and_control_characters(self):
        sample = b.reading("Headset\n\033", 10, identity="bluetooth:000000000003")
        hosts = [host("a", sample), host("b", sample)]
        report = dict(hosts=hosts, devices=b.merge_devices(hosts), warnings=[], generated_at=b.now())
        output = io.StringIO()
        with redirect_stdout(output):
            b.render(report)
        text = output.getvalue()
        self.assertEqual(text.count("Headset"), 1)
        self.assertIn("a, b", text)
        self.assertIn("1 unique", text)
        self.assertNotIn("\033", text)


if __name__ == "__main__":
    unittest.main()
