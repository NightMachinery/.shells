"""Private download diagnostics and optional YouTube sign-in."""
from io import StringIO
from pathlib import Path
import re
import tempfile

COOKIE_LIMIT = 64 * 1024
COOKIE_HEADER = "# Netscape HTTP Cookie File"
HTTP_ONLY = "#HttpOnly_"
URL = re.compile(r"https?://[^\s<>]+", re.I)
SECRET_LINE = re.compile(r"\b(?:cookie|set-cookie|authorization)\s*:.*", re.I)
TERMINAL_ESCAPE = re.compile(r"\x1b\[[0-?]*[ -/]*[@-~]")


def safe_log(message):
    message = TERMINAL_ESCAPE.sub("", str(message))
    lines = [SECRET_LINE.sub("[redacted header]", line) for line in message.splitlines()]
    return URL.sub("[URL]", " ".join(lines))[:1200]


def youtube_needs_login(error):
    text = str(error).lower()
    return "sign in to confirm" in text or "login required" in text or "log in to confirm" in text


class DownloadLogger:
    def __init__(self, record):
        self.record = record

    def debug(self, message):
        # yt-dlp sends normal output through debug, prefixed verbose diagnostics
        # separately. The latter can include request details and stays disabled.
        if not message.startswith(("[debug]", "[download] ")):
            self.record("info", message)

    def warning(self, message):
        self.record("warning", message)

    def error(self, message):
        self.record("error", message)


class YouTubeCookies:
    def __init__(self, state):
        self.path = Path(state) / "secrets" / "youtube.cookies.txt"

    def status(self):
        return {"configured": self.path.is_file()}

    def stream(self):
        try:
            return StringIO(self.path.read_text(encoding="utf-8"))
        except FileNotFoundError:
            return None

    def remove(self):
        self.path.unlink(missing_ok=True)

    def import_file(self, text):
        if not isinstance(text, str) or len(text.encode("utf-8", errors="replace")) > COOKIE_LIMIT:
            raise ValueError("Choose a cookie export of at most 64 KB")
        lines = text.removeprefix("\ufeff").splitlines()
        if not lines or lines[0].strip() not in (COOKIE_HEADER, "# HTTP Cookie File"):
            raise ValueError("Choose a Netscape-format cookies.txt export")
        selected = []
        for line in lines[1:]:
            entry = line.removeprefix(HTTP_ONLY)
            if not entry.strip() or entry.startswith("#"):
                continue
            fields = entry.split("\t")
            if len(fields) != 7 or re.search(r"[\x00-\x08\x0b-\x1f\x7f]", entry):
                raise ValueError("The cookie export contains an invalid row")
            domain, subdomains, path, secure, expires, name, value = fields
            host = domain.lower().removeprefix(".")
            if host != "youtube.com" and not host.endswith(".youtube.com"):
                continue
            if (not re.fullmatch(r"\.?[a-zA-Z0-9.-]+", domain)
                    or subdomains != ("TRUE" if domain.startswith(".") else "FALSE")
                    or secure not in ("TRUE", "FALSE") or not path.startswith("/")
                    or (expires and not re.fullmatch(r"[0-9]+(?:\.[0-9]+)?", expires))
                    or not re.fullmatch(r"[!#$%&'*+.^_`|~0-9A-Za-z-]+", name)
                    or not all(field.isascii() for field in (path, value))):
                raise ValueError("The cookie export contains an invalid YouTube row")
            selected.append(line)
        if not selected:
            raise ValueError("No youtube.com cookies were found in the export")

        self.path.parent.mkdir(mode=0o700, parents=True, exist_ok=True)
        temporary = None
        try:
            with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", delete=False,
                                             dir=self.path.parent) as output:
                temporary = Path(output.name)
                output.write("\n".join([COOKIE_HEADER, *selected, ""]))
            temporary.replace(self.path)
        finally:
            if temporary is not None:
                temporary.unlink(missing_ok=True)
