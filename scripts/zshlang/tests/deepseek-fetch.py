#!/usr/bin/env python3
"""Exercise the zsh Fetch wrapper without contacting a network service."""

import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]


class FetchTests(unittest.TestCase):
    def run_fetch(self, *urls, key="synthetic-test-key", mode="ok"):
        with tempfile.TemporaryDirectory() as directory:
            curl = Path(directory) / "curl"
            curl.write_text('''#!/usr/bin/env python3
import json,os,sys
from pathlib import Path
args=sys.argv[1:]
headers=Path(args[args.index('--header')+1][1:]).read_text()
assert 'Authorization: Bearer synthetic-test-key' in headers
assert 'Content-Type: application/json' in headers
assert not any('synthetic-test-key' in arg for arg in args)
assert args[-1]=='https://api.deepseek.com/v1/tools/fetch'
url=json.loads(args[args.index('--data')+1])['url']
if os.environ['TEST_FETCH_MODE']=='fail': sys.exit(22)
if os.environ['TEST_FETCH_MODE']=='invalid': print('{}')
else: print(json.dumps({'title':'Example','content':'Page: '+url+'\\n\\n'}))
''')
            curl.chmod(0o755)
            env = dict(os.environ, TEST_FETCH_BIN=directory, TEST_FETCH_KEY=key,
                       TEST_FETCH_MODE=mode)
            return subprocess.run(["zsh", "-c", 'path=("${TEST_FETCH_BIN}" "${path[@]}"); deepseek_api_key="${TEST_FETCH_KEY}"; deepseek-fetch "$@"',
                                   "_", *urls], env=env, text=True, capture_output=True, timeout=20)

    def test_single_url_returns_content(self):
        result = self.run_fetch("https://example.org/one")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "Page: https://example.org/one\n")

    def test_multiple_urls_have_exact_separators(self):
        result = self.run_fetch("https://example.org/one", "https://example.org/two")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "URL: https://example.org/one\n\nPage: https://example.org/one"
                         "\n\n---\n\nURL: https://example.org/two\n\nPage: https://example.org/two\n")

    def test_missing_key_and_missing_urls(self):
        for urls, key in ((("https://example.org",), ""), ((), "synthetic-test-key")):
            result = self.run_fetch(*urls, key=key)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")

    def test_transport_and_bad_payload_fail_without_content(self):
        for mode in ("fail", "invalid"):
            result = self.run_fetch("https://example.org", mode=mode)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(result.stdout, "")
            self.assertNotIn("synthetic-test-key", result.stderr)


if __name__ == "__main__":
    unittest.main()
