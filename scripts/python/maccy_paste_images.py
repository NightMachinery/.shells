"""Export image payloads from Maccy's Core Data SQLite store without changing it."""

import os
import re
import sqlite3
import sys
import tempfile
from datetime import datetime
from pathlib import Path


CORE_DATA_EPOCH = 978307200


def export_images(count: int, database: Path, destination: Path) -> None:
    if not database.is_file():
        raise ValueError(f"Maccy history database not found: {database}")
    if not destination.is_dir():
        raise ValueError(f"output directory not found: {destination}")

    connection = sqlite3.connect(f"{database.resolve().as_uri()}?mode=ro", uri=True)
    try:
        connection.execute("PRAGMA query_only = ON")
        connection.execute("BEGIN")
        items = connection.execute(
            """
            SELECT i.Z_PK, COALESCE(i.ZLASTCOPIEDAT, i.ZFIRSTCOPIEDAT, 0)
            FROM ZHISTORYITEM AS i
            WHERE EXISTS (
                SELECT 1 FROM ZHISTORYITEMCONTENT AS c
                WHERE c.ZITEM = i.Z_PK
                  AND c.ZTYPE IN ('public.png', 'public.tiff')
                  AND LENGTH(c.ZVALUE) > 0
            )
            ORDER BY COALESCE(i.ZLASTCOPIEDAT, i.ZFIRSTCOPIEDAT, 0) DESC,
                     i.Z_PK DESC
            LIMIT ?
            """,
            (count,),
        ).fetchall()

        for item_id, copied_at in items:
            image_type, payload = connection.execute(
                """
                SELECT ZTYPE, ZVALUE FROM ZHISTORYITEMCONTENT
                WHERE ZITEM = ? AND ZTYPE IN ('public.png', 'public.tiff')
                  AND LENGTH(ZVALUE) > 0
                ORDER BY CASE ZTYPE WHEN 'public.png' THEN 0 ELSE 1 END,
                         Z_PK DESC
                LIMIT 1
                """,
                (item_id,),
            ).fetchone()
            extension = ".png" if image_type == "public.png" else ".tiff"
            stamp = datetime.fromtimestamp(copied_at + CORE_DATA_EPOCH).strftime(
                "%Y%m%d-%H%M%S"
            )
            output = destination / f"maccy-{stamp}-{item_id}{extension}"
            if output.exists():
                print(f"maccy-paste-images: already exists, skipped: {output}", file=sys.stderr)
                continue
            temporary = None
            try:
                with tempfile.NamedTemporaryFile(dir=destination, prefix=".maccy-", delete=False) as target:
                    temporary = Path(target.name)
                    target.write(payload)
                os.link(temporary, output)
            except FileExistsError:
                print(f"maccy-paste-images: already exists, skipped: {output}", file=sys.stderr)
                continue
            finally:
                if temporary is not None:
                    temporary.unlink(missing_ok=True)
            print(output)
    finally:
        connection.close()


def main() -> int:
    if len(sys.argv) != 4 or not re.fullmatch(r"[0-9]+", sys.argv[1]):
        print("maccy-paste-images: n must be a positive integer", file=sys.stderr)
        return 2
    try:
        count = int(sys.argv[1])
    except ValueError:
        print("maccy-paste-images: n must be a positive integer", file=sys.stderr)
        return 2
    if not 1 <= count <= 2**63 - 1:
        print("maccy-paste-images: n must be a positive integer", file=sys.stderr)
        return 2
    try:
        export_images(count, Path(sys.argv[2]), Path(sys.argv[3]))
    except (OSError, sqlite3.Error, ValueError, OverflowError) as error:
        print(f"maccy-paste-images: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
