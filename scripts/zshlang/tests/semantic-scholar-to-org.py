#!/usr/bin/env python3
"""Shell contracts for the native Go paper helper (parser/render tests live in Go)."""
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
URL = "https://www.semanticscholar.org/paper/" + "a" * 40

class ShellContracts(unittest.TestCase):
    def run_shell(self, function, *, input_url=URL, fail=False):
        with tempfile.TemporaryDirectory() as directory:
            binary = Path(directory) / "org-paper-metadata"
            binary.write_text("""#!/usr/bin/env python3
import json,os,sys
if os.environ['TEST_PAPER_FAIL']:
    print('api failed: synthetic failure', file=sys.stderr)
    sys.exit(22)
print(json.dumps({'args':sys.argv[1:],'key_present':bool(os.getenv('SemanticScholar_API_key')),'scraper':os.getenv('semantic_scholar_scraper')}))
""")
            binary.chmod(0o755)
            updater = Path(directory) / "org-paper-metadata-updater"
            updater.write_bytes(binary.read_bytes())
            updater.chmod(0o755)
            env = dict(os.environ, TEST_PAPER_BIN=directory, TEST_PAPER_FAIL="y" if fail else "")
            script = r'''
path=("${TEST_PAPER_BIN}" "${path[@]}")
function h-org-paper-tools-dep { return 0 }
function ecgray { print -ru2 -- "GRAY:$*" }
function pbcopy { print -ru2 -- 'Unexpected clipboard write'; return 99 }
SemanticScholar_API_key=synthetic-key
deepseek_api_key=synthetic-key
semantic_scholar_scraper=page-data
"$1" "$2"
'''
            return subprocess.run(["zsh", "-c", script, "_", function, input_url], env=env,
                                  text=True, capture_output=True, timeout=25)

    def test_json_delegates_to_go_and_exports_configuration(self):
        result=self.run_shell("semantic-scholar-to-json")
        self.assertEqual(result.returncode,0,result.stderr)
        data=json.loads(result.stdout)
        self.assertEqual(data['args'],['--format','json',URL])
        self.assertTrue(data['key_present'])
        self.assertEqual(data['scraper'],'page-data')

    def test_org_delegates_without_lisp(self):
        result=self.run_shell("semantic-scholar-to-org")
        self.assertEqual(result.returncode,0,result.stderr)
        self.assertEqual(json.loads(result.stdout)['args'],['--format','org',URL])

    def test_resolver_delegates_to_native_head_redirect(self):
        result=self.run_shell("h-semantic-scholar-page-url",input_url='https://arxiv.org/abs/2505.00001')
        self.assertEqual(result.returncode,0,result.stderr)
        self.assertEqual(json.loads(result.stdout)['args'],['--resolve','https://arxiv.org/abs/2505.00001'])

    def test_failure_is_gray_and_returns_no_org(self):
        result=self.run_shell("semantic-scholar-to-org",fail=True)
        self.assertEqual(result.returncode,22,result.stderr)
        self.assertEqual(result.stdout,'')
        self.assertIn('GRAY:api failed:',result.stderr)

    def test_updater_delegates_to_go_and_preserves_root_argument(self):
        result=self.run_shell("org-paper-metadata-updater",input_url="/tmp/synthetic root")
        self.assertEqual(result.returncode,0,result.stderr)
        data=json.loads(result.stdout)
        self.assertEqual(data['args'],['/tmp/synthetic root'])
        self.assertTrue(data['key_present'])

    def test_explicit_api_remains_available(self):
        result=self.run_shell("semantic-scholar-to-json-api")
        self.assertEqual(result.returncode,0,result.stderr)
        self.assertEqual(json.loads(result.stdout)['args'][:4],['--format','json','--scraper','api'])

if __name__=='__main__':
    unittest.main()
