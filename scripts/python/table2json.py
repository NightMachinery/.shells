#!/usr/bin/env python3
#: @untested @GPT4
##
from bs4 import BeautifulSoup
import json
import sys
from pynight.common_icecream import ic


def html_table_to_json(html_content):
    soup = BeautifulSoup(html_content, "html.parser")
    table = soup.find("table")
    headers = [th.text for th in table.select("tr th")]
    ic(table, headers)

    table_data = []
    for row in table.select("tr")[1:]:
        rowData = {}
        for idx, cell in enumerate(row.select("td")):
            rowData[headers[idx]] = cell.text
        table_data.append(rowData)

    return json.dumps(table_data, indent=4)


def main():
    html_content = sys.stdin.read()
    json_data = html_table_to_json(html_content)
    print(json_data)


if __name__ == "__main__":
    main()
