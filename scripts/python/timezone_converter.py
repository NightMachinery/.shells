#!/usr/bin/env python3
##
#: developed using G25
##

import argparse
import re
# Need timezone for fixed offsets
from datetime import datetime, time, timedelta, date, timezone
import sys
import io # Needed to capture print output for tests
import traceback # For printing exceptions during testing

# Global variable used by the test runner to inject arguments
args_to_parse_for_test = None

# --- Dependency Check ---
try:
    # ZoneInfo is essential
    from zoneinfo import ZoneInfo, ZoneInfoNotFoundError
except ImportError:
    print("FATAL Error: 'zoneinfo' module required (standard in Python 3.9+).", file=sys.stderr)
    print("Install 'tzdata' package (e.g., 'sudo apt install tzdata' or 'pip install tzdata') "
          "or ensure Python 3.9+.", file=sys.stderr)
    sys.exit(2) # Exit if dependency missing

# --- Explicit Mappings ---
# Case-insensitive mapping for specific inputs
# Value is either a direct (timedelta, description) tuple for fixed standard time offsets,
# or the IANA string to look up for locations or DST-sensitive abbreviations/zones.
EXPLICIT_TZ_MAP = {
    # --- Fixed Standard Offsets (Does NOT account for DST where applicable) ---
    "utc": (timedelta(0), "Timezone 'UTC' (Fixed UTC+00:00)"),
    "gmt": (timedelta(0), "Timezone 'GMT' (Fixed UTC+00:00)"),
    "hst": (timedelta(hours=-10), "Timezone 'HST' (Hawaii Standard Time, Fixed UTC-10:00, No DST)"),
    "akst": (timedelta(hours=-9), "Timezone 'AKST' (Alaska Standard Time, Fixed UTC-09:00)"),
    "pst": (timedelta(hours=-8), "Timezone 'PST' (Pacific Standard Time, Fixed UTC-08:00)"),
    "mst": (timedelta(hours=-7), "Timezone 'MST' (Mountain Standard Time, Fixed UTC-07:00)"), # Note: Arizona specific case below
    "cst": (timedelta(hours=-6), "Timezone 'CST' (Central Standard Time, Fixed UTC-06:00)"),
    "est": (timedelta(hours=-5), "Timezone 'EST' (Eastern Standard Time, Fixed UTC-05:00)"),
    "wet": (timedelta(hours=0), "Timezone 'WET' (Western European Time, Fixed UTC+00:00)"),
    "cet": (timedelta(hours=1), "Timezone 'CET' (Central European Time, Fixed UTC+01:00)"),
    "eet": (timedelta(hours=2), "Timezone 'EET' (Eastern European Time, Fixed UTC+02:00)"),
    "jst": (timedelta(hours=9), "Timezone 'JST' (Japan Standard Time, Fixed UTC+09:00)"),
    "aest": (timedelta(hours=10), "Timezone 'AEST' (Australian Eastern Standard Time, Fixed UTC+10:00)"),
    "awst": (timedelta(hours=8), "Timezone 'AWST' (Australian Western Standard Time, Fixed UTC+08:00, No DST)"),
    # India Standard Time (Fixed Offset) - Use a distinct key to avoid 'ist' ambiguity
    "ist_india": (timedelta(hours=5, minutes=30), "Timezone 'IST' (India Standard Time, Fixed UTC+05:30)"),

    # --- Location / DST-Aware Abbreviation / Country Mappings (Lookup current offset via IANA name) ---

    # North America - Cities & Regions
    "new york": "America/New_York", # EST/EDT
    "newyork": "America/New_York",
    "nyc": "America/New_York",
    "los angeles": "America/Los_Angeles", # PST/PDT
    "losangeles": "America/Los_Angeles",
    "la": "America/Los_Angeles",
    "chicago": "America/Chicago", # CST/CDT
    "houston": "America/Chicago",
    "dallas": "America/Chicago",
    "denver": "America/Denver",   # MST/MDT
    "phoenix": "America/Phoenix", # MST fixed, no DST (most of state)
    "arizona": "America/Phoenix",
    "seattle": "America/Los_Angeles", # PST/PDT
    "san francisco": "America/Los_Angeles", # PST/PDT
    "sf": "America/Los_Angeles",
    "miami": "America/New_York",  # EST/EDT
    "boston": "America/New_York",
    "washington": "America/New_York", # Washington D.C.
    "dc": "America/New_York",
    "atlanta": "America/New_York",
    "toronto": "America/Toronto", # EST/EDT
    "montreal": "America/Toronto",
    "ottawa": "America/Toronto",
    "vancouver": "America/Vancouver", # PST/PDT
    "calgary": "America/Edmonton", # MST/MDT
    "edmonton": "America/Edmonton",
    "winnipeg": "America/Winnipeg", # CST/CDT
    "mexico city": "America/Mexico_City", # CST potentially with DST

    # North America - General Zones / DST abbreviations
    "et": "America/New_York",      # Eastern Time (EST/EDT)
    "eastern": "America/New_York",
    "edt": "America/New_York",     # Eastern Daylight Time
    "pt": "America/Los_Angeles",   # Pacific Time (PST/PDT)
    "pacific": "America/Los_Angeles",
    "pdt": "America/Los_Angeles",  # Pacific Daylight Time
    "ct": "America/Chicago",       # Central Time (CST/CDT)
    "central": "America/Chicago",
    "cdt": "America/Chicago",      # Central Daylight Time
    "mt": "America/Denver",        # Mountain Time (MST/MDT) - Use Denver as default DST observer
    "mountain": "America/Denver",
    "mdt": "America/Denver",       # Mountain Daylight Time
    "akt": "America/Anchorage",    # Alaska Time (AKST/AKDT)
    "alaska": "America/Anchorage",
    "akdt": "America/Anchorage",   # Alaska Daylight Time
    "hawaii": "Pacific/Honolulu",  # HST fixed UTC-10

    # Europe - Cities
    "london": "Europe/London",     # Handles GMT/BST (WET/WEST)
    "paris": "Europe/Paris",       # Handles CET/CEST
    "berlin": "Europe/Berlin",     # Handles CET/CEST
    "madrid": "Europe/Madrid",     # Handles CET/CEST
    "barcelona": "Europe/Madrid",  # Handles CET/CEST
    "rome": "Europe/Rome",         # Handles CET/CEST
    "lisbon": "Europe/Lisbon",     # Handles WET/WEST
    "dublin": "Europe/Dublin",     # Handles GMT/IST (Irish Summer Time = UTC+1)
    "amsterdam": "Europe/Amsterdam", # Handles CET/CEST
    "brussels": "Europe/Brussels", # Handles CET/CEST
    "zurich": "Europe/Zurich",     # Handles CET/CEST
    "geneva": "Europe/Zurich",
    "vienna": "Europe/Vienna",     # Handles CET/CEST
    "copenhagen": "Europe/Copenhagen", # Handles CET/CEST
    "stockholm": "Europe/Stockholm", # Handles CET/CEST
    "oslo": "Europe/Oslo",         # Handles CET/CEST
    "helsinki": "Europe/Helsinki", # Handles EET/EEST
    "warsaw": "Europe/Warsaw",     # Handles CET/CEST
    "athens": "Europe/Athens",     # Handles EET/EEST
    "moscow": "Europe/Moscow",     # MSK Fixed UTC+3
    "istanbul": "Europe/Istanbul", # Turkey Time (TRT)
    "kyiv": "Europe/Kyiv",         # EET/EEST

    # Europe - General Zones / DST abbreviations
    "bst": "Europe/London",        # British Summer Time (=WEST)
    "ist_irish": "Europe/Dublin",  # Irish Standard Time (Summer, = UTC+1)
    "west": "Europe/Lisbon",       # Western European Summer Time (matches London/Lisbon DST)
    "cest": "Europe/Paris",        # Central European Summer Time
    "eest": "Europe/Helsinki",     # Eastern European Summer Time

    # Asia - Cities & Regions
    "tehran": "Asia/Tehran",       # Iran Standard Time
    "iran": "Asia/Tehran",
    "dubai": "Asia/Dubai",         # Gulf Standard Time (Fixed UTC+4)
    "kolkata": "Asia/Kolkata",     # India Standard Time (IST, Fixed UTC+5:30)
    "mumbai": "Asia/Kolkata",
    "delhi": "Asia/Kolkata",
    "tokyo": "Asia/Tokyo",         # Japan Standard Time (JST, Fixed UTC+9)
    "seoul": "Asia/Seoul",         # Korea Standard Time (KST, Fixed UTC+9)
    "beijing": "Asia/Shanghai",    # China Standard Time (CST, Fixed UTC+8)
    "shanghai": "Asia/Shanghai",
    "hong kong": "Asia/Hong_Kong", # Hong Kong Time (HKT, Fixed UTC+8)
    "singapore": "Asia/Singapore", # Singapore Time (SGT, Fixed UTC+8)

    # Australia/Oceania - Cities
    "sydney": "Australia/Sydney",      # Handles AEST/AEDT
    "melbourne": "Australia/Melbourne",# Handles AEST/AEDT
    "brisbane": "Australia/Brisbane",  # AEST fixed, no DST
    "perth": "Australia/Perth",        # AWST fixed, no DST
    "auckland": "Pacific/Auckland",    # NZST/NZDT

    # Australia - DST abbreviations
    "aedt": "Australia/Sydney",     # Australian Eastern Daylight Time

    # Africa - Cities
    "cairo": "Africa/Cairo",        # EET potentially with DST changes
    "lagos": "Africa/Lagos",        # WAT (Fixed UTC+1)
    "johannesburg": "Africa/Johannesburg", # SAST (Fixed UTC+2)

    # South America - Cities
    "sao paulo": "America/Sao_Paulo", # Brazil Time (BRT/BRST - DST varies)
    "rio de janeiro": "America/Sao_Paulo",
    "buenos aires": "America/Argentina/Buenos_Aires", # Argentina Time (ART - Fixed UTC-3)

    # Country Mappings (to Capital or Primary Zone)
    "usa": "America/New_York",
    "united states": "America/New_York",
    "canada": "America/Toronto", # Common default, acknowledges regional variation exists
    "mexico": "America/Mexico_City",
    "uk": "Europe/London",
    "united kingdom": "Europe/London",
    "france": "Europe/Paris",
    "germany": "Europe/Berlin",
    "spain": "Europe/Madrid",
    "italy": "Europe/Rome",
    "ireland": "Europe/Dublin",
    "portugal": "Europe/Lisbon",
    "switzerland": "Europe/Zurich",
    "austria": "Europe/Vienna",
    "belgium": "Europe/Brussels",
    "netherlands": "Europe/Amsterdam",
    "denmark": "Europe/Copenhagen",
    "norway": "Europe/Oslo",
    "sweden": "Europe/Stockholm",
    "finland": "Europe/Helsinki",
    "poland": "Europe/Warsaw",
    "greece": "Europe/Athens",
    "turkey": "Europe/Istanbul",
    "russia": "Europe/Moscow", # Defaulting to Moscow time
    "japan": "Asia/Tokyo",
    "china": "Asia/Shanghai", # Officially one timezone
    "india": "Asia/Kolkata",  # IST
    "australia": "Australia/Sydney", # Common default
    "new zealand": "Pacific/Auckland",
    "brazil": "America/Sao_Paulo", # Common default
    "argentina": "America/Argentina/Buenos_Aires",
}
# Note: Mapping generic "IST" is avoided due to ambiguity (India vs Ireland vs Israel).

# --- Helper Functions ---
def format_timedelta_offset(offset: timedelta | None) -> str:
    """Formats a timedelta offset into a string like +HH:MM or -HH:MM."""
    if offset is None: return "+00:00"
    total_seconds = int(offset.total_seconds())
    sign = '+' if total_seconds >= 0 else '-'
    total_seconds = abs(total_seconds)
    hours, remainder = divmod(total_seconds, 3600)
    minutes, _ = divmod(remainder, 60)
    return f"{sign}{hours:02d}:{minutes:02d}"

def parse_timezone(original_tz_str: str) -> tuple[timedelta | None, str, timezone | ZoneInfo | None]:
    """
    Parses a timezone string (offset, name, specific mapped term).
    Returns tuple: (current UTC offset timedelta, descriptive string, zone object).
    The zone object is either a datetime.timezone (for fixed offsets) or
    zoneinfo.ZoneInfo (for IANA names). Returns None for offset/object on failure.
    Raises ValueError for invalid formats or unknown names.
    """
    tz_str = original_tz_str.strip()
    tz_str_lower = tz_str.lower()
    zone_object = None # Initialize

    # 1. Check Explicit Mappings (using lowercase input)
    if tz_str_lower in EXPLICIT_TZ_MAP:
        map_value = EXPLICIT_TZ_MAP[tz_str_lower]
        if isinstance(map_value, tuple): # Fixed offset case (UTC, GMT, PST, EST, etc.)
            offset_delta, description = map_value
            try:
                # Use the original string's case (or upper) if it matches the key, else use the key's case
                tz_name = original_tz_str.strip().upper() if original_tz_str.strip().lower() == tz_str_lower else tz_str_lower.upper()
                zone_object = timezone(offset_delta, tz_name)
            except Exception:
                zone_object = timezone(offset_delta) # Fallback if name causes issue
            return offset_delta, description, zone_object
        else: # Mapped to an IANA name (Iran, PDT, London, ET, France, etc.)
            iana_name = map_value
            try:
                zone = ZoneInfo(iana_name)
                zone_object = zone # Store the ZoneInfo object
                now_aware = datetime.now(zone)
                offset_delta = now_aware.utcoffset()
                if offset_delta is None: raise ValueError(f"Offset calculation failed for ZoneInfo '{iana_name}'.")

                formatted_offset = format_timedelta_offset(offset_delta)
                description = (f"Timezone '{iana_name}' (currently UTC{formatted_offset})"
                               f" <-- Interpreted '{original_tz_str}' as '{iana_name}'")
                if zone.key.startswith("Etc/GMT"):
                    description += " (Note: Etc/GMT follows POSIX sign convention)"
                return offset_delta, description, zone_object
            except ZoneInfoNotFoundError:
                 raise ValueError(f"Internal Error: Mapped IANA timezone '{iana_name}' (from '{original_tz_str}') not found.") from None
            except Exception as e:
                 raise ValueError(f"Error processing timezone '{iana_name}' (mapped from '{original_tz_str}'): {e}") from e

    # 2. Try parsing as an offset (e.g., +2, -5:30, 0)
    if tz_str == '0': # Handle '0' explicitly as offset
        offset_delta = timedelta(0)
        zone_object = timezone(offset_delta, "UTC") # Use UTC name for 0 offset
        return offset_delta, "Offset +00:00 from UTC (interpreted from '0')", zone_object

    # Regex allows +H, +HH, +H:MM, +HH:MM and negative versions
    offset_match = re.fullmatch(r"([+-])(\d{1,2})(?:[:]?(\d{2}))?", tz_str)
    if offset_match:
        sign = -1 if offset_match.group(1) == '-' else 1
        hours = int(offset_match.group(2))
        minutes = int(offset_match.group(3)) if offset_match.group(3) else 0

        if not (0 <= hours <= 14):
            raise ValueError(f"Invalid offset value: '{original_tz_str}'. Hours must be between 0 and 14.")
        if not (0 <= minutes <= 59):
            raise ValueError(f"Invalid offset value: '{original_tz_str}'. Minutes must be between 00 and 59.")
        if hours == 14 and minutes != 0:
             raise ValueError(f"Invalid offset value: '{original_tz_str}'. Maximum offset is +/- 14:00.")

        offset_delta = timedelta(hours=sign * hours, minutes=sign * minutes)
        formatted_offset_str = format_timedelta_offset(offset_delta)
        desc = f"Offset {formatted_offset_str} from UTC (interpreted from '{original_tz_str}')"
        try:
            # Create timezone object for the fixed offset
            zone_object = timezone(offset_delta, f"UTC{formatted_offset_str}")
        except Exception:
            zone_object = timezone(offset_delta) # Fallback
        return offset_delta, desc, zone_object

    # 3. Try direct ZoneInfo lookup for IANA names or possibly resolvable abbreviations
    try:
        zone = ZoneInfo(tz_str)
        zone_object = zone # Store the ZoneInfo object
    except ZoneInfoNotFoundError:
         if '/' not in tz_str and tz_str.isupper() and 2 <= len(tz_str) <= 5:
              raise ValueError(f"Timezone abbreviation '{original_tz_str}' not recognized or ambiguous. "
                               f"Consider using an explicitly mapped term (e.g., PST, EST, PDT, CEST), "
                               f"a full IANA name (e.g., Europe/Paris, America/New_York), or an offset (e.g., -08:00, +01:00).") from None
         else:
             raise ValueError(f"Invalid timezone '{original_tz_str}'. Not an offset, mapped term, country, city, or known IANA name.") from None
    except Exception as e:
         raise ValueError(f"Error initializing timezone '{original_tz_str}': {e}") from e

    # --- If ZoneInfo(tz_str) succeeded without explicit mapping ---
    try:
        now_aware = datetime.now(zone)
        offset_delta = now_aware.utcoffset()
        if offset_delta is None: raise ValueError(f"Offset calculation failed for ZoneInfo '{zone.key}'.")

        formatted_offset = format_timedelta_offset(offset_delta)
        description = f"Timezone '{zone.key}' (currently UTC{formatted_offset})"
        if zone.key.startswith("Etc/GMT"):
             description += " (Note: Etc/GMT follows POSIX sign convention)"

        input_looks_like_abbr = '/' not in tz_str and tz_str.isupper() and 2 <= len(tz_str) <= 5
        if tz_str != zone.key:
            description += f" <-- Interpreted '{original_tz_str}' as '{zone.key}'"
        elif input_looks_like_abbr and tz_str == zone.key:
            description += f" <-- Abbreviation '{original_tz_str}' resolved directly"

        return offset_delta, description, zone_object
    except Exception as e:
        raise ValueError(f"Error calculating offset for timezone '{zone.key}' (from input '{original_tz_str}'): {e}") from e


# --- UPDATED parse_time function ---
def parse_time(time_str: str) -> time:
    """
    Parses HH:MM, HH, or HH:MMam/pm, Ham/pm time strings.
    Returns time object. Raises ValueError.
    Handles case-insensitivity and optional space for am/pm.
    """
    original_input = time_str # Keep for error messages
    time_str = time_str.strip()
    time_str_lower = time_str.lower()
    indicator = None

    # Check for AM/PM indicator (case-insensitive)
    if time_str_lower.endswith('am'):
        indicator = 'am'
        # Remove 'am', potentially with preceding space
        time_part = re.sub(r'\s*am$', '', time_str, flags=re.IGNORECASE).strip()
    elif time_str_lower.endswith('pm'):
        indicator = 'pm'
        # Remove 'pm', potentially with preceding space
        time_part = re.sub(r'\s*pm$', '', time_str, flags=re.IGNORECASE).strip()
    else:
        # No indicator found, assume 24-hour format
        time_part = time_str

    try:
        hour = 0
        minute = 0

        # Parse the time part (which is now stripped of am/pm)
        if ':' in time_part:
            parts = time_part.split(':')
            if len(parts) == 2 and parts[0].isdigit() and parts[1].isdigit() and len(parts[1]) == 2:
                hour = int(parts[0])
                minute = int(parts[1])
                # Validate minutes universally
                if not (0 <= minute <= 59):
                     raise ValueError("Minute (0-59) out of range.")
                # Validate hour based on whether am/pm was present
                if indicator: # AM/PM hour validation (1-12)
                    if not (1 <= hour <= 12):
                        raise ValueError("Hour must be between 1 and 12 for AM/PM format.")
                else: # 24-hour validation (0-23)
                    if not (0 <= hour <= 23):
                        raise ValueError("Hour must be between 0 and 23 for 24-hour format.")
            else:
                # More specific error for bad HH:MM structure
                if not (parts[0].isdigit() and parts[1].isdigit()):
                     raise ValueError("Hour and Minute parts must be digits in HH:MM format.")
                elif len(parts[1]) != 2:
                     raise ValueError("Minutes must be two digits (e.g., 05, not 5) in HH:MM format.")
                else: # General fallback
                     raise ValueError("Invalid HH:MM structure.")

        elif time_part.isdigit():
            hour = int(time_part)
            minute = 0 # Default minute to 0 for HH format
            # Validate hour based on whether am/pm was present
            if indicator: # AM/PM hour validation (1-12)
                if not (1 <= hour <= 12):
                    raise ValueError("Hour must be between 1 and 12 for AM/PM format when using H or HH.")
            else: # 24-hour validation (0-23)
                if not (0 <= hour <= 23):
                    raise ValueError("Hour must be between 0 and 23 for 24-hour format when using HH.")
        else:
            # If it's not HH:MM and not just digits, it's invalid
            raise ValueError("Time part must be digits (HH) or in HH:MM format.")

        # --- Apply AM/PM Conversion if necessary ---
        if indicator == 'am':
            if hour == 12: # 12 AM (Midnight) corresponds to 00 hour
                hour = 0
            # Hours 1-11 AM remain unchanged
        elif indicator == 'pm':
            if hour != 12: # 1 PM to 11 PM correspond to 13-23 hours
                hour += 12
            # Hour 12 PM (Noon) remains 12

        # Return the final time object
        return time(hour, minute)

    except ValueError as e:
        # Re-raise with the original input string for better context
        raise ValueError(f"Invalid time format or value: '{original_input}'. {e}") from e
    except Exception as e: # Catch potential unexpected errors (e.g., regex issues)
        raise ValueError(f"Unexpected error parsing time '{original_input}': {e}") from e


# --- Test Suite Logic ---
def run_test(test_args, expect_success, expected_output_contains=None):
    """Runs the main logic via main_wrapper with args, checking exit code and output."""
    print(f"\n>>> Testing with args: {test_args} (Expect: {'Success' if expect_success else 'Failure'})")
    global args_to_parse_for_test
    args_to_parse_for_test = test_args # Inject args for main_wrapper
    original_stdout = sys.stdout
    original_stderr = sys.stderr
    captured_stdout = io.StringIO()
    captured_stderr = io.StringIO()
    sys.stdout = captured_stdout
    sys.stderr = captured_stderr
    exit_code = 99 # Default error code for unexpected non-exit

    try:
        # Call main_wrapper which encapsulates the logic and should call sys.exit
        main_wrapper(test_mode=True)
        # If main_wrapper returns without SystemExit, it's an error in the test setup/logic
        print("\n!!! TEST ERROR: main_wrapper did not exit as expected! !!!", file=original_stderr)
        exit_code = 98 # Indicate unexpected normal return
    except SystemExit as e:
        exit_code = e.code if e.code is not None else 1 # Capture the exit code from sys.exit
    except Exception as e:
        # Catch any other unexpected exceptions during the test run
        print(f"\n!!! UNEXPECTED EXCEPTION DURING TEST: {type(e).__name__} - {e} !!!", file=original_stderr)
        traceback.print_exc(file=original_stderr)
        exit_code = 99 # Indicate unexpected exception
    finally:
        # Restore stdout/stderr and reset test args
        sys.stdout = original_stdout
        sys.stderr = original_stderr
        args_to_parse_for_test = None

    # Get captured output
    stdout_val = captured_stdout.getvalue().strip()
    stderr_val = captured_stderr.getvalue().strip()
    if stdout_val: print(f"--- Captured STDOUT ---\n{stdout_val}")
    if stderr_val: print(f"--- Captured STDERR ---\n{stderr_val}")

    # Determine if the test passed based on exit code vs expectation
    actual_success = (exit_code == 0)
    test_passed = (actual_success == expect_success)

    # Optional content check (only if the test passed based on exit code so far)
    if test_passed and expect_success and expected_output_contains:
        combined_output = stdout_val + "\n" + stderr_val
        if expected_output_contains not in combined_output:
            # Refined check for key parts if full string not found
            key_part_match = re.search(r"'(.*?)'", expected_output_contains)
            key_part = key_part_match.group(1) if key_part_match else None
            offset_part_match = re.search(r"Offset ([+-]\d{2}:\d{2})", expected_output_contains)
            offset_part = offset_part_match.group(1) if offset_part_match else None

            found_key_part = key_part and key_part in combined_output
            found_offset_part = offset_part and offset_part in combined_output

            if not found_key_part and not found_offset_part:
                failure_reason = f"Expected output to contain '{expected_output_contains}'."
                if key_part: failure_reason += f" Did not find key part '{key_part}' either."
                if offset_part: failure_reason += f" Did not find offset part '{offset_part}' either."
                print(f"!!! FAILED (Content Check): {failure_reason}")
                test_passed = False # Override pass based on content mismatch

    # Final verification: If failure was expected, non-zero exit is required for PASS.
    if not expect_success and exit_code == 0:
        test_passed = False # Failure expected, but got success code 0
    elif not expect_success and exit_code != 0:
        test_passed = True # Failure expected, and got non-zero code (Correct!)

    # If success was expected, exit code MUST be 0.
    elif expect_success and exit_code != 0:
        test_passed = False # Success expected, but got non-zero code


    print(f"--- Test Result: {'PASSED' if test_passed else 'FAILED'} ---")
    print("="*40)
    return test_passed


# --- UPDATED run_test_suite function ---
def run_test_suite():
    """Executes the categorized test suite and reports results."""
    print("--- Running Internal Test Suite ---")

    # --- Test Cases Definition ---
    # Conversion (3 args) - Success
    basic_offset_success = [
        ['+2', '+3:30', '11:00'], ['-5', '+1', '14:00'],
        ['+05:30', '-04:00', '08:00'], ['0', '+1', '00:00'],
    ]
    explicit_fixed_success = [
        ['UTC', 'CET', '10:00'], ['PST', 'EST', '10:00'],
        ['ist_india', 'UTC', '05:30'],
    ]
    explicit_iana_lookup_success = [
        ['Iran', 'UTC', '12:00'], ['PDT', 'CEST', '09:00'],
        ['PT', 'ET', '10:00'], ['London', 'New York', '12:00'],
        ['UK', 'Japan', '08:00'], ['USA', 'China', '21:00'],
    ]
    iana_direct_success = [
        ['America/New_York', 'Asia/Tokyo', '10:00'],
        ['Pacific/Kiritimati', 'Etc/GMT+12', '12:00'],
    ]
    whitespace_time_format_success = [
         [' +1 ', ' -2:00 ', ' 9 '], [' UTC ', ' Europe/Paris ', ' 10:00 '],
    ]
    # --- NEW: AM/PM Success Cases ---
    ampm_success = [
        ['EST', 'Tehran', '1:30pm'], # Your original example
        ['EST', 'Tehran', '1:30 PM'], # Uppercase PM, space
        ['UTC', 'PST', '9am'],       # Lowercase am, no space
        ['UTC', 'PST', '09 AM'],     # Uppercase AM, space, leading zero
        ['CET', 'JST', '12:00pm'],   # 12 PM (Noon)
        ['CET', 'JST', '12:00 am'],  # 12 AM (Midnight)
        ['+1', '+2', '12pm'],        # 12 PM hour only
        ['+1', '+2', '12am'],        # 12 AM hour only
        ['+1', '+2', '1pm'],         # 1 PM hour only
        ['+1', '+2', '1am'],         # 1 AM hour only
    ]
    conversion_success_cases = (basic_offset_success + explicit_fixed_success + explicit_iana_lookup_success +
                                iana_direct_success + whitespace_time_format_success + ampm_success) # Added ampm_success

    # Current Time (1 arg) - Success (No changes needed here as it doesn't parse time)
    current_time_success = [
        (['+03:00'], "Offset +03:00 from UTC"),
        (['-08'], "Offset -08:00 from UTC"),
        (['UTC'], "Timezone 'UTC'"),
        (['PST'], "Timezone 'PST'"),
        (['America/Denver'], "Timezone 'America/Denver'"),
        (['London'], "Timezone 'Europe/London'"),
        (['ET'], "Timezone 'America/New_York'"),
        (['Japan'], "Timezone 'Asia/Tokyo'"),
        (['ist_india'], "Timezone 'IST' (India"),
        ([' +05:30 '], "Offset +05:30 from UTC"),
        ([' paris '], "Timezone 'Europe/Paris'"),
    ]

    # Failure Cases
    invalid_time_failures = [ # Invalid time value in 3-arg mode
        ['+1', '+2', '25:00'], ['+1', '+2', 'abc'], ['+1', '+2', '9:5'],
        ['+1', '+2', '14:60'],
    ]
    # --- NEW: AM/PM Failure Cases ---
    ampm_failure = [
        ['+1', '+2', '13:00pm'], # Hour > 12 with PM
        ['+1', '+2', '0:00am'],  # Hour 0 with AM (should be 12am)
        ['+1', '+2', '1:30 xm'], # Invalid indicator
        ['+1', '+2', '1:60pm'],  # Invalid minute
        ['+1', '+2', '1am pm'],  # Double indicator
        ['+1', '+2', 'am'],      # Indicator only
        ['+1', '+2', '1:MMam'],  # Non-digit minute
    ]
    invalid_tz_failures = [ # Invalid timezone value in 3-arg mode
        ['+15:00', '+2', '10:00'], ['Bogus/Zone', '+1', '10:00'], ['+2:XX', '+1', '10:00'],
        ['ZZZ', 'UTC', '10:00'], ['IST', 'UTC', '12:00'], # Ambiguous IST
    ]
    invalid_tz_single_arg_failures = [ # Invalid timezone value in 1-arg mode
        ['+15:00'], ['Bogus/Zone'], ['+2:XX'], ['ZZZ'],
        ['IST'], # Ambiguous IST
        ['NowhereLand'],
    ]
    # Test cases expecting exit code 1 due to wrong arg count
    wrong_arg_count_failures = [
        [],
        ['+1', '+2'],
        ['+1','+2','+3','+4'],
    ]

    # Combine failure cases
    conversion_failure_cases = invalid_time_failures + invalid_tz_failures + ampm_failure # Added ampm_failure
    failure_test_cases = (conversion_failure_cases + invalid_tz_single_arg_failures +
                          wrong_arg_count_failures)

    # --- Run Tests ---
    success_passed = 0
    failure_passed = 0
    total_success_conv = len(conversion_success_cases)
    total_success_curr = len(current_time_success)
    total_success = total_success_conv + total_success_curr
    total_failure = len(failure_test_cases)

    print("\n--- Running Success Tests (Conversion - 3 Args) ---")
    for i, case in enumerate(conversion_success_cases):
        print(f"\n--- Conversion Success Test {i+1}/{total_success_conv} ---")
        if run_test(case, expect_success=True):
            success_passed += 1

    print("\n--- Running Success Tests (Current Time - 1 Arg) ---")
    for i, (case, expected_out) in enumerate(current_time_success):
         print(f"\n--- Current Time Success Test {i+1}/{total_success_curr} ---")
         if run_test(case, expect_success=True, expected_output_contains=expected_out):
             success_passed += 1

    print("\n--- Running Failure Tests ---")
    for i, case in enumerate(failure_test_cases):
        print(f"\n--- Failure Test {i+1}/{total_failure} ---")
        if run_test(case, expect_success=False): # Expecting non-zero exit code
            failure_passed += 1

    # --- Report Summary ---
    print("\n--- Test Suite Summary ---")
    print(f"Success Tests: {success_passed} / {total_success} PASSED" + (" ✨" if success_passed == total_success else ""))
    print(f"Failure Tests: {failure_passed} / {total_failure} Correctly Failed" + (" ✨" if failure_passed == total_failure else ""))
    print("--- Internal Test Suite Finished ---")


# --- Core Logic Functions ---
def show_current_time(tz_input: str) -> int:
    """Parses timezone and prints current time. Returns exit code."""
    print("--- Current Time Mode ---")
    try:
        _offset, description, zone_obj = parse_timezone(tz_input)
        if zone_obj is None:
            # This case might occur if parse_timezone fails gracefully, but shouldn't with current impl.
            raise ValueError(f"Could not create a valid timezone object for '{tz_input}'.")

        now_in_tz = datetime.now(zone_obj)
        print(f"Showing current time for: {description}")
        print("-" * 26)
        # Format time, handling abbreviation vs offset name for clarity
        tz_abbr = now_in_tz.strftime('%Z')
        # Use offset string if abbreviation is missing, numeric, or just 'UTC' (which is less specific than offset sometimes)
        if not tz_abbr or tz_abbr.startswith(('+', '-')) or tz_abbr == "UTC":
             # Use standard offset format like UTC+HH:MM
            tz_abbr = f"UTC{now_in_tz.strftime('%z')[:-2]}:{now_in_tz.strftime('%z')[-2:]}"

        print(f"Current Time: {now_in_tz.strftime('%Y-%m-%d %H:%M:%S')} {tz_abbr} ({now_in_tz.strftime('%z')})")
        return 0 # Success exit code
    except ValueError as e:
        # Handle errors during timezone parsing
        print(f"Input Error: {e}", file=sys.stderr)
        return 1 # Failure exit code for input errors
    except Exception as e:
        # Handle unexpected errors during time calculation/formatting
        print(f"\nAn unexpected error occurred getting current time: {type(e).__name__} - {e}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        return 3 # Different exit code for internal errors

def convert_time(source_tz_input: str, target_tz_input: str, time_val_input: str) -> int:
    """Parses inputs and performs time conversion. Returns exit code."""
    print("--- Time Conversion Mode ---")
    try:
        # Parse source and target timezones, and the input time
        source_offset, source_desc, _ = parse_timezone(source_tz_input)
        target_offset, target_desc, _ = parse_timezone(target_tz_input)
        input_time = parse_time(time_val_input) # Uses the updated parse_time

        # Ensure offsets were successfully determined (parse funcs raise ValueError on failure)
        if source_offset is None or target_offset is None:
             # This check is somewhat redundant if parse_timezone always raises ValueError on failure
             print("Error: Could not determine offsets for provided timezones.", file=sys.stderr)
             return 1 # Exit code 1 for input/parsing errors

        # Print interpretation details for user clarity
        print(f"Source: {source_desc}")
        print(f"Target: {target_desc}")
        print("-" * 20)

        # Perform the time calculation using offset difference
        # Combine the parsed time with today's date to create a naive datetime object
        base_dt_naive = datetime.combine(date.today(), input_time)
        # Calculate the difference between target and source UTC offsets
        time_difference = target_offset - source_offset
        # Apply the difference to the naive base datetime
        target_dt = base_dt_naive + time_difference

        # Format for output
        input_time_formatted = input_time.strftime("%H:%M") # Always output in 24hr
        target_time_formatted = target_dt.strftime("%H:%M") # Always output in 24hr

        # Determine if the day changed and create appropriate suffix string
        days_diff = (target_dt.date() - base_dt_naive.date()).days
        day_change_str = ""
        if days_diff == 1:
            day_change_str = " (next day)"
        elif days_diff == -1:
            day_change_str = " (previous day)"
        elif days_diff != 0:
            # Handle cases crossing more than one day boundary
            day_change_str = f" ({days_diff:+} days)"

        # Display results
        print(f"Input Time:  {input_time_formatted} (in '{source_tz_input}')")
        print(f"Output Time: {target_time_formatted} (in '{target_tz_input}'){day_change_str}")
        return 0 # Success exit code
    except ValueError as e:
        # Handle errors from parse_timezone or parse_time
        print(f"Input Error: {e}", file=sys.stderr)
        return 1 # Failure exit code for input errors
    except Exception as e:
        # Handle unexpected errors during calculation/formatting
        print(f"\nAn unexpected error occurred during conversion: {type(e).__name__} - {e}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        return 3 # Different exit code for internal errors


# --- UPDATED create_help_parser function ---
def create_help_parser():
    """Creates a minimal parser primarily for generating the help message."""
    parser = argparse.ArgumentParser(
        prog="timezone_converter.py",
        description="Convert a time between timezones OR show the current time in a specific timezone.",
        formatter_class=argparse.RawTextHelpFormatter,
        add_help=False, # Use custom help action below
        # Define usage string explicitly for clarity
        usage="%(prog)s [-h] {<timezone> | <source_tz> <target_tz> <TIME> | test}",
        epilog="""Modes:
  <timezone>              Show current time in the specified timezone.
  <source_tz> <target_tz> <TIME>
                          Convert TIME from source_tz to target_tz.
  test                    Run internal test suite.

Examples (Current Time):
  %(prog)s UTC
  %(prog)s America/New_York
  %(prog)s London
  %(prog)s +05:30

Examples (Conversion):
  %(prog)s +2 +3:30 11:00
  %(prog)s +05:30 -04:00 08:00
  %(prog)s PST EST 10:00
  %(prog)s London 'New York' 14
  %(prog)s EST Tehran "1:30 pm"   # Use quotes if time has spaces
  %(prog)s UTC PST 9am
  %(prog)s CET JST 12:00PM

Notes:
- Timezones can be offsets (+HH:MM, -HH, +H), IANA names (e.g., Europe/Paris),
  common abbreviations (UTC, PST, EST, PDT, CEST), cities (London, Tokyo),
  or countries (UK, France, Japan - defaults to capital/primary zone).
- Standard time abbreviations (PST, EST, CET) use fixed offsets and DO NOT adjust for DST.
- DST abbreviations (PDT, EDT, CEST), locations (London, New York), general
  zone names (ET, PT), and country/city names use the CURRENT actual offset, handling DST.
- Time for conversion can be HH:MM (e.g., 14:05), HH (e.g., 9 -> 09:00),
  or use AM/PM like H:MMam, HH:MMpm, Ham, HHpm (e.g., 1:30pm, 9am, 12 PM).
  AM/PM is case-insensitive and ignores space before it.
"""
    )
    # Add the standard help action, but suppress it from appearing as a positional arg
    parser.add_argument('-h', '--help', action='help', default=argparse.SUPPRESS,
                        help='Show this help message and exit')
    # Add dummy positional args solely for the usage line generated by action='help'
    # These are NOT used by the main dispatch logic.
    parser.add_argument("arg1", nargs='?', help=argparse.SUPPRESS)
    parser.add_argument("arg2", nargs='?', help=argparse.SUPPRESS)
    parser.add_argument("arg3", nargs='?', help=argparse.SUPPRESS)
    return parser

# --- Main Entry Point & Dispatch Logic ---
def main_wrapper(test_mode=False):
    """
    Handles initial argument checks (test, help) and dispatches
    to the appropriate logic based on argument count. Calls sys.exit().
    This is the main entry point called by tests and the __main__ block.
    """
    # Determine arguments: use test args if provided and in test_mode, else use sys.argv
    if test_mode and args_to_parse_for_test is not None:
         # In test mode, sys.argv[0] is not relevant, only use injected args
         script_name = "timezone_converter.py" # Dummy script name for error messages
         args_list = args_to_parse_for_test
    else:
         # Normal execution or case where test args weren't provided
         script_name = sys.argv[0]
         args_list = sys.argv[1:]

    num_args = len(args_list)

    # Handle 'test' argument (only if run as 'python script.py test')
    # This check prevents the test suite running automatically when imported.
    if num_args == 1 and args_list[0].lower() == 'test' and not test_mode:
        print("Running test suite based on 'test' argument...")
        run_test_suite()
        sys.exit(0) # Exit cleanly after running tests

    # Handle '-h' or '--help' explicitly
    # We do this before the main count check to ensure help takes precedence.
    help_parser = create_help_parser() # Create parser instance
    if '-h' in args_list or '--help' in args_list:
        help_parser.print_help(sys.stdout)
        sys.exit(0) # Exit cleanly after showing help

    # Dispatch based on argument count for core functionality
    exit_code = 1 # Default to error code 1 (usage error)
    try:
        if num_args == 1:
            # Single argument: Show Current Time Mode
            exit_code = show_current_time(args_list[0])
        elif num_args == 3:
            # Three arguments: Time Conversion Mode
            exit_code = convert_time(args_list[0], args_list[1], args_list[2])
        else:
            # Incorrect number of arguments (0, 2, >3)
            print(f"{script_name}: error: incorrect number of arguments ({num_args} provided).", file=sys.stderr)
            print("Use one argument for current time, three for conversion, or --help.", file=sys.stderr)
            # Optionally print usage line from help parser for more guidance
            # help_parser.print_usage(sys.stderr)
            exit_code = 1 # Set specific code for usage error
    except Exception as e:
        # Catch any unexpected errors from the core logic functions
        print(f"\nAn unexpected error occurred: {type(e).__name__} - {e}", file=sys.stderr)
        traceback.print_exc(file=sys.stderr)
        exit_code = 3 # Use a different exit code for internal errors
    finally:
        # Ensure sys.exit is always called to terminate the process with the determined code
        # This is crucial for the test runner (run_test) which catches SystemExit.
        sys.exit(exit_code)

# --- Standard Script Execution Entry Point ---
if __name__ == "__main__":
    # This block runs only when the script is executed directly (e.g., python timezone_converter.py ...)
    # It calls the main wrapper function, setting test_mode to False.
    main_wrapper(test_mode=False)

# Note: The run_test_suite() function is defined above but is only called
# automatically if the script is invoked with the single argument 'test',
# or when explicitly called by a testing framework or the tool environment.
