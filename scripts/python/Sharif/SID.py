#!/usr/bin/env python3

import argparse
import json
import sys
import requests
import os
from bs4 import BeautifulSoup

import getpass


# --- Configuration ---
BASE = "https://net2.sharif.edu"
LOGIN_URL = f"{BASE}/en-us/user/login/"
CONNECT_URL = f"{BASE}/en-us/user/aaa_ras_connect/"
PORTAL_LOGOUT_URL = f"{BASE}/logout"
META_URL = f"{BASE}/en-us/user/get_user_metadata/"
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
    session.headers.update({
        "User-Agent": ("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                       "AppleWebKit/537.36 (KHTML, like Gecko) "
                       "Chrome/139.0.0.0 Safari/537.36")
    })
    return session


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


def print_user_info(session):
    """Fetches and prints Net2 user metadata for the authenticated session."""
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

    # Print results
    print("\n✅ Login Successful! User Information:")
    print(f"  Username           : {username}")
    print(f"  Creation Date      : {creation_date}")
    print(f"  Remaining Volume   : {credit}")
    print(f"  Remaining Time     : {deposit}")
    print(f"  Expiration Date    : {exp_date}")
    print("\nFinished successfully.")


def logout(session):
    """Logs out of the active Net2 portal session without prompting for credentials."""
    logout_resp = session.post(PORTAL_LOGOUT_URL, data="", headers={"Referer": BASE}, timeout=10)
    logout_resp.raise_for_status()

    print("Logged out successfully.")


def main():
    parser = argparse.ArgumentParser(description="Sharif Net2 login/logout and user info")
    subparsers = parser.add_subparsers(dest="command")
    subparsers.add_parser("login", help="log in and print user info")
    subparsers.add_parser("logout", help="log out from Sharif Net2")
    args = parser.parse_args()

    session = build_session()

    try:
        if args.command == "logout":
            logout(session)
        else:
            username, password = get_credentials()
            login_session(session, username, password, connect=True)
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
