#!/usr/bin/env python3
"""Add or replace provider entries in a Paseo daemon's config.json.

  paseo_providers_set.py CONFIG ENTRIES_JSON

ENTRIES_JSON maps provider ids to entries ({"extends": "acp", ...}). Only those
ids under agents.providers change; every other key is kept, and so are any
options someone already set on a replaced entry (the new options win). The
file is written atomically, with the previous version kept as CONFIG.bak.

Paseo cannot set one provider through `paseo daemon config set` (dynamic keys
are not addressable), and setting the whole map would drop entries other tools
wrote, so this edits the file and the caller runs `paseo daemon reload`.
"""

import json
import os
import re
import shutil
import sys
import tempfile

#: Paseo's own rule for provider ids (packages/protocol/src/provider-config.ts).
PROVIDER_ID = re.compile(r"[a-z][a-z0-9-]*")


def main() -> int:
    if len(sys.argv) != 3:
        print(__doc__.strip(), file=sys.stderr)
        return 2
    path, entries = sys.argv[1], json.loads(sys.argv[2])
    if not isinstance(entries, dict) or not entries:
        print("paseo_providers_set: ENTRIES_JSON must be a non-empty object", file=sys.stderr)
        return 2
    for provider, entry in entries.items():
        if not PROVIDER_ID.fullmatch(provider) or not isinstance(entry, dict):
            print(f"paseo_providers_set: invalid provider: {provider}", file=sys.stderr)
            return 2
    with open(path, encoding="utf-8") as stream:
        config = json.load(stream)
    providers = config.setdefault("agents", {}).setdefault("providers", {})
    for provider, entry in entries.items():
        kept = (providers.get(provider) or {}).get("options") or {}
        entry = dict(entry)
        entry["options"] = {**kept, **(entry.get("options") or {})}
        providers[provider] = entry

    directory = os.path.dirname(os.path.abspath(path))
    shutil.copy2(path, path + ".bak")
    fd, temporary = tempfile.mkstemp(prefix=".config-", dir=directory)
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as stream:
            json.dump(config, stream, indent=2)
            stream.write("\n")
        os.chmod(temporary, os.stat(path).st_mode & 0o777)
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)
    return 0


if __name__ == "__main__":
    sys.exit(main())
