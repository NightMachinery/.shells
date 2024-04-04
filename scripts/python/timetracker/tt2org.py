#!/usr/bin/env python3
##
import argparse
from collections import OrderedDict
from types import SimpleNamespace
from datetime import datetime, timedelta
import sqlite3
import os
import jdatetime
from pynight.common_icecream import ic


def connect_to_db(db_path):
    conn = sqlite3.connect(db_path)
    cursor = conn.cursor()
    cursor.execute("SELECT * FROM activity")
    rows = cursor.fetchall()
    activities = [
        SimpleNamespace(id=row[0], name=row[1], start=row[2], end=row[3])
        for row in rows
    ]
    conn.close()

    # Sort activities based on the start date
    sorted_activities = sorted(activities, key=lambda x: parse_datetime(x.start))

    return sorted_activities


def parse_datetime(date_str):
    try:
        return datetime.strptime(date_str, "%Y-%m-%d %H:%M:%S.%f")
    except ValueError:
        return datetime.strptime(date_str, "%Y-%m-%d %H:%M:%S")


def merge_activities(activities):
    merged = []
    prev_activity = activities[0]

    for activity in activities[1:]:
        time_diff = parse_datetime(activity.start) - parse_datetime(prev_activity.end)

        # Check if the current activity name matches the previous one and time difference is less than a minute
        if activity.name == prev_activity.name and time_diff <= timedelta(minutes=1):
            prev_activity.end = activity.end
        else:
            merged.append(prev_activity)
            prev_activity = activity

    merged.append(prev_activity)
    return merged


def group_activities_by_date(activities):
    grouped = OrderedDict()

    for activity in activities:
        start_dt = parse_datetime(activity.start)

        # If the time is before 5:30 AM, consider it as part of the previous day
        if start_dt.time() < datetime.strptime("05:30", "%H:%M").time():
            date_key = start_dt.date() - timedelta(days=1)
        else:
            date_key = start_dt.date()

        if date_key not in grouped:
            grouped[date_key] = []

        grouped[date_key].append(activity)

    return grouped


def format_to_org_mode(
    grouped_activities,
    group_by,
    ##
    # act_prefix="- ",
    act_prefix="",
    ##
    # act_sep=" :: ",
    # act_sep=" : ",
    act_sep="   ",
    ##
):
    org_entries = []

    if group_by == "none":
        for date, activities in grouped_activities.items():
            # Convert Gregorian date to Jalali date
            jalali_date = jdatetime.date.fromgregorian(date=date)
            weekday_name = date.strftime("%A")
            date_header = f"* {weekday_name} [jalali:{jalali_date.year}/{jalali_date.month}/{jalali_date.day}] [gregorian:{date.year}/{date.month}/{date.day}]"
            org_entries.append(date_header)

            for activity in activities:
                start_time = parse_datetime(activity.start).strftime("%H:%M")
                end_time = parse_datetime(activity.end).strftime("%H:%M")
                org_entries.append(
                    f"{act_prefix}{start_time} -> {end_time}{act_sep}{activity.name}"
                )
            org_entries.append("")

    elif group_by == "jalali":
        current_year = None
        current_month = None
        for date, activities in grouped_activities.items():
            jalali_date = jdatetime.date.fromgregorian(date=date)
            if current_year != jalali_date.year:
                current_year = jalali_date.year
                org_entries.append(f"* {current_year}")
            if current_month != jalali_date.month:
                current_month = jalali_date.month
                month_name = jalali_date.strftime("%B")
                org_entries.append(f"** {month_name}{current_month}")

            weekday_name = date.strftime("%A")
            date_header = f"*** {weekday_name} [jalali:{jalali_date.year}/{jalali_date.month}/{jalali_date.day}] [gregorian:{date.year}/{date.month}/{date.day}]"
            org_entries.append(date_header)
            for activity in activities:
                start_time = parse_datetime(activity.start).strftime("%H:%M")
                end_time = parse_datetime(activity.end).strftime("%H:%M")
                org_entries.append(
                    f"{act_prefix}{start_time} -> {end_time}{act_sep}{activity.name}"
                )
            org_entries.append("")

    return "\n".join(org_entries)


##
def main():
    # Argument parsing
    parser = argparse.ArgumentParser(
        description="Convert timetracker data to org-mode format."
    )
    parser.add_argument(
        "--head-n",
        type=int,
        default=None,
        help="Number of lines to display from the top of the org-mode output",
    )
    parser.add_argument(
        "--group-by",
        choices=["jalali", "none", "greg"],
        default="jalali",
        help="Group activities by either Jalali or Gregorian date",
    )
    parser.add_argument(
        "--db-path",
        type=str,
        default=os.environ.get("timetracker_db", "/path/to/your/database"),
        help="Path to the SQLite database file",
    )
    args = parser.parse_args()

    # Retrieve and process activities
    activities = connect_to_db(args.db_path)
    merged_activities = merge_activities(activities)
    grouped_activities = group_activities_by_date(merged_activities)

    # Format to org-mode based on grouping choice
    org_mode_output = format_to_org_mode(grouped_activities, args.group_by)

    # Display the specified number of lines from the top of the org-mode output
    print("\n".join(org_mode_output.split("\n")[: args.head_n]))


if __name__ == "__main__":
    main()
