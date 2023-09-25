#!/usr/bin/env python3
#: @deprecated
##
import argparse
import os
from pathlib import Path
from convertdate import persian
import glob
import datetime
from dateutil.relativedelta import relativedelta
from pynight.common_icecream import ic
from concurrent.futures import ThreadPoolExecutor


def rem_comingup(*, reminders_dir, until=31):
    today = datetime.date.today()

    def process_day(i):
        future_date = today + relativedelta(days=i)
        future_text = f"{i} day(s) later: {future_date.strftime('%Y/%m/%d (%a)')}"

        rem_today = get_rem_today(
            future_date,
            reminders_dir=reminders_dir,
        )
        if rem_today:
            future_text += f"\n{rem_today}"
            return future_text
        return None

    with ThreadPoolExecutor(
        # max_workers=12,
    ) as executor:
        results = list(executor.map(process_day, range(1, until + 1)))

    return "\n\n".join(filter(None, results))


def get_rem_today(date, reminders_dir):
    # Convert the date to Jalali
    year, month, day = persian.from_gregorian(date.year, date.month, date.day)

    # Format the date string
    date_str = f"{year}/{month:02d}/{day:02d}"

    # Create the glob pattern
    glob_pattern = Path(reminders_dir) / f"{date_str}*"

    # Find the file(s) matching the pattern
    matches = list(glob.glob(str(glob_pattern)))

    # If a match is found, read and return its content
    if matches:
        with open(matches[0], "r") as f:
            return f.read()

    # Return an empty string if no matches
    return ""


def main():
    reminders_dir = os.environ.get("remindayDir")

    parser = argparse.ArgumentParser(description="Reminday helpers")
    subparsers = parser.add_subparsers(dest="command")

    comingup_parser = subparsers.add_parser("comingup", help="Show upcoming reminders")
    comingup_parser.add_argument("days", type=int, help="Number of days to show")
    comingup_parser.add_argument("--reminders_dir", help="Path to reminders directory")

    args = parser.parse_args()

    if args.command == "comingup":
        reminders_dir = args.reminders_dir or reminders_dir
        print(
            rem_comingup(
                until=args.days,
                reminders_dir=reminders_dir,
            )
        )


if __name__ == "__main__":
    main()
