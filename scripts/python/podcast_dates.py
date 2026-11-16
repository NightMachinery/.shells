"""Original YouTube upload dates shared by the CLI and web podcast feed writers."""
import datetime
from functools import lru_cache
import re

JALALI_MONTHS = ("Farvardin", "Ordibehesht", "Khordad", "Tir", "Mordad", "Shahrivar",
                "Mehr", "Aban", "Azar", "Dey", "Bahman", "Esfand")
GREGORIAN_MONTHS = ("January", "February", "March", "April", "May", "June", "July",
                    "August", "September", "October", "November", "December")
WEEKDAYS = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")


def parse_upload_date(value):
    """Accept a complete date; never substitute a download date or a year alone."""
    if isinstance(value, datetime.datetime):
        return value.date()
    if isinstance(value, datetime.date):
        return value
    if not isinstance(value, str):
        return None
    if not re.fullmatch(r"\d{8}|\d{4}-\d{2}-\d{2}", value.strip()):
        return None
    try:
        return datetime.datetime.strptime(value.strip().replace("-", ""), "%Y%m%d").date()
    except ValueError:
        return None


@lru_cache(maxsize=4096)
def format_upload_date(value):
    date = parse_upload_date(value)
    if date is None:
        return ""
    import jdatetime
    jalali = jdatetime.date.fromgregorian(date=date)
    # Same month-name/month-number and weekday layout as datej-all-long (now),
    # without a clock time: upload_date metadata has date precision only.
    return (f"{jalali.year}/{JALALI_MONTHS[jalali.month-1]}{jalali.month}/{jalali.day} "
            f"{WEEKDAYS[date.weekday()]} {date.day}/{GREGORIAN_MONTHS[date.month-1]}{date.month}/{date.year}")


def append_upload_date(description, value):
    description = description or ""
    suffix = format_upload_date(value)
    if not suffix:
        return description
    description = re.sub(r"(?:\n\s*)?Original upload date: [^\n]*\s*$", "", description).rstrip()
    return (description + "\n\n" if description else "") + "Original upload date: " + suffix
