#!/usr/bin/env python3

import argparse
import json
import sys
import requests
import os
from pathlib import Path
from bs4 import BeautifulSoup

import getpass


# --- Configuration ---
BASE = "https://net2.sharif.edu"
LOGIN_URL = f"{BASE}/en-us/user/login/"
CONNECT_URL = f"{BASE}/en-us/user/aaa_ras_connect/"
PORTAL_LOGOUT_URL = f"{BASE}/logout"
META_URL = f"{BASE}/en-us/user/get_user_metadata/"
ONLINE_SESSION_URL = f"{BASE}/en-us/user/get_user_online_session/"
COOKIE_FILE = Path(os.environ.get("sharif_sid_cookie_file", "~/.cache/sharif/SID.cookies.json")).expanduser()
# ---------------------

def humanize_credit(raw_credit: str) -> str:
    """Converts raw credit (in KB) to a human-readable GB string."""
    try:
        # Credit is typically in KB, convert to GB
        credit_gb = float(int(raw_credit) / 1024)
        if f"{credit_gb:.3f}" == "0.001":
            return "Unlimited"
        return f"{credit_gb:.3f} GB"
    except Exception:
        return raw_credit

def humanize_deposit(raw_deposit) -> str:
    """Converts raw deposit (in seconds) to a human-readable Hour string."""
    try:
        if raw_deposit and int(raw_deposit) > 0:
            # Deposit is typically in seconds, convert to hours
            hours = int(int(raw_deposit) / 3600)
            return f"{hours} Hour(s)"
        return "Unlimited"
    except Exception:
        return str(raw_deposit)

def get_credentials():
    """Returns Sharif VPN credentials from the environment or interactive prompts."""
    username = os.environ.get("sharif_vpn_username")
    password = os.environ.get("sharif_vpn_passowrd")

    if not username or not password:
        print("---")
        print("Note: sharif_vpn_username or sharif_vpn_passowrd environment variables not found.")
        print("Falling back to manual input.")
        print("---")
        username = input("User: ")
        # getpass is used for secure password input (hides typing)
        password = getpass.getpass("Password: ")
    else:
        print("---")
        print("Using credentials from sharif_vpn_username and sharif_vpn_passowrd environment variables.")
        print("---")

    return username, password


def build_session():
    """Builds a requests session with the browser-like headers Net2 expects."""
    session = requests.Session()
    session.trust_env = False
    session.headers.update({
        "User-Agent": ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                       "AppleWebKit/537.36 (KHTML, like Gecko) "
                       "Chrome/139.0.0.0 Safari/537.36")
    })
    return session


def load_cookies(session):
    """Loads persisted Net2 session cookies, if present."""
    if not COOKIE_FILE.exists():
        return

    try:
        cookies = json.loads(COOKIE_FILE.read_text())
    except Exception as e:
        print(f"Warning: Could not read saved session cookies. ({e})", file=sys.stderr)
        return

    if isinstance(cookies, dict):
        session.cookies.update(cookies)


def save_cookies(session):
    """Persists Net2 session cookies without storing credentials."""
    COOKIE_FILE.parent.mkdir(parents=True, exist_ok=True)
    cookie_data = requests.utils.dict_from_cookiejar(session.cookies)
    flags = os.O_WRONLY | os.O_CREAT | os.O_TRUNC
    with os.fdopen(os.open(COOKIE_FILE, flags, 0o600), "w") as cookie_file:
        json.dump(cookie_data, cookie_file)


def clear_cookies():
    """Removes persisted Net2 session cookies."""
    try:
        COOKIE_FILE.unlink()
    except FileNotFoundError:
        pass


def login_session(session, username, password, connect=True):
    """Logs in to Net2 and optionally activates the RAS connection."""
    # Step 1: Get CSRF token from login page
    resp = session.get(LOGIN_URL)
    resp.raise_for_status()
    soup = BeautifulSoup(resp.text, "html.parser")
    csrf_tag = soup.find("input", {"name": "csrfmiddlewaretoken"})
    if not csrf_tag:
        print("Error: Could not find CSRF token on login page.", file=sys.stderr)
        sys.exit(1)

    csrf = csrf_tag["value"]

    # Step 2: Post login credentials
    login_data = {
        "csrfmiddlewaretoken": csrf,
        "username": username,
        "password": password,
    }
    session.headers.update({"Referer": LOGIN_URL})
    login_resp = session.post(LOGIN_URL, data=login_data, allow_redirects=True)
    login_resp.raise_for_status()

    if not connect:
        return

    # Step 3: Attempt to connect (simulate the internal JS call)
    # This is often needed to get the session fully authenticated/active
    connect_headers = {
        "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
        "X-CSRFToken": session.cookies.get("csrftoken", ""),
        "X-Requested-With": "XMLHttpRequest",
        "Referer": LOGIN_URL,
    }
    connect_data = {"user": username, "pass": password}
    try:
        conn = session.post(CONNECT_URL, data=connect_data, headers=connect_headers, timeout=10)
        conn.raise_for_status()
    except requests.RequestException as e:
        # Connection attempt often fails or returns non-200 but might still set the session correctly
        print(f"Warning: Connection attempt failed or timed out, proceeding to check metadata. ({e})", file=sys.stderr)


def get_user_info(session):
    """Fetches Net2 user metadata for the authenticated session."""
    # Step 4: Get User Metadata
    meta_headers = {"X-Requested-With": "XMLHttpRequest", "Referer": BASE}
    meta = session.get(META_URL, headers=meta_headers, timeout=15)
    meta.raise_for_status()

    # Check for successful login/metadata retrieval
    if "user/login" in meta.url:
        print("Login failed: Session redirected back to the login page.", file=sys.stderr)
        sys.exit(1)

    if not meta.text.strip():
        print("Error: Empty metadata response (authentication likely failed).", file=sys.stderr)
        sys.exit(1)

    payload = json.loads(meta.text)
    result = payload.get("result", {})
    if not result:
        # Check for specific error messages from the payload if possible
        error_message = payload.get("error_msg")
        if error_message:
            print(f"Login or Metadata retrieval failed: {error_message}", file=sys.stderr)
        else:
            print("Error: No 'result' in metadata (Authentication or API issue).", file=sys.stderr)
        sys.exit(1)

    user_id = next(iter(result.keys()))
    info = result[user_id]

    # Extract and humanize data
    username = info.get("normal_username", "").split("@")[0]
    creation_date = info.get("creation_date", "N/A")
    exp_date = info.get("exp_date", "N/A")
    credit = humanize_credit(info.get("credit", "0"))
    deposit = humanize_deposit(info.get("deposit"))

    return {
        "username": username,
        "creation_date": creation_date,
        "exp_date": exp_date,
        "credit": credit,
        "deposit": deposit,
    }


def print_user_info(session, title="Login Successful! User Information"):
    """Prints Net2 user metadata for the authenticated session."""
    info = get_user_info(session)

    # Print results
    print(f"\n✅ {title}:")
    print(f"  Username           : {info['username']}")
    print(f"  Creation Date      : {info['creation_date']}")
    print(f"  Remaining Volume   : {info['credit']}")
    print(f"  Remaining Time     : {info['deposit']}")
    print(f"  Expiration Date    : {info['exp_date']}")
    print("\nFinished successfully.")


def logout(session):
    """Logs out of the active Net2 portal session without prompting for credentials."""
    load_cookies(session)
    logout_resp = session.post(
        PORTAL_LOGOUT_URL,
        data="",
        headers={"Referer": BASE},
        timeout=10,
        allow_redirects=False,
    )
    logout_resp.raise_for_status()
    clear_cookies()

    print("Logged out successfully.")


def print_status(session):
    """Prints the current Net2 portal status without changing login state."""
    load_cookies(session)
    status_resp = session.get(
        ONLINE_SESSION_URL,
        headers={"X-Requested-With": "XMLHttpRequest", "Referer": BASE},
        timeout=10,
        allow_redirects=False,
    )

    if status_resp.is_redirect:
        location = status_resp.headers.get("Location", "")
        if "user/login" in location:
            clear_cookies()
            print("Logged out.")
            return
        print_user_info(session, title="Status")
        return

    status_resp.raise_for_status()

    if not status_resp.text.strip():
        clear_cookies()
        print("Logged out.")
        return

    payload = json.loads(status_resp.text)
    result = payload.get("result", {})
    if not result:
        print("Logged in.")
        print("Disconnected.")
        return

    if isinstance(result, list):
        online_sessions = []
        for item in result:
            if isinstance(item, list):
                online_sessions.extend(entry for entry in item if isinstance(entry, dict))
            elif isinstance(item, dict):
                online_sessions.append(item)
    else:
        user_id = next(iter(result.keys()))
        online_sessions = result.get(user_id) or []

    current_ip = payload.get("ip")
    current_session = next(
        (entry for entry in online_sessions if entry.get("session_ip") == current_ip),
        None,
    )

    print("Logged in.")
    if current_session:
        print("Connected.")
        print(f"  Session IP         : {current_session.get('session_ip', current_ip)}")
        print(f"  Login Date         : {current_session.get('session_start_time', 'N/A')}")
    else:
        print("Disconnected.")

    if online_sessions:
        print(f"  Active Sessions    : {len(online_sessions)}")


def main():
    parser = argparse.ArgumentParser(description="Sharif Net2 login/logout/status and user info")
    subparsers = parser.add_subparsers(dest="command")
    subparsers.add_parser("login", help="log in and print user info")
    subparsers.add_parser("logout", help="log out from Sharif Net2")
    subparsers.add_parser("status", help="print Sharif Net2 status")
    args = parser.parse_args()

    session = build_session()

    try:
        if args.command == "logout":
            logout(session)
        elif args.command == "status":
            print_status(session)
        else:
            username, password = get_credentials()
            login_session(session, username, password, connect=True)
            save_cookies(session)
            print_user_info(session)

    except requests.exceptions.HTTPError as e:
        print(f"\n❌ HTTP Error occurred: {e}", file=sys.stderr)
        print("Check if the website is accessible and if credentials are correct.", file=sys.stderr)
        sys.exit(1)
    except requests.exceptions.RequestException as e:
        print(f"\n❌ Connection Error: {e}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"\n❌ An unexpected error occurred: {e}", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
