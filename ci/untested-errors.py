#!/usr/bin/env python3
"""List the error checks in the library that no test reaches, as Markdown.

Usage: untested-errors.py <gcovr JSON report>

An error check is a line that sets an error code or an error message. The generated sources in src/ repeat each
check for every kind, so a check counts as reached when any of its copies is. What's left is a check no test makes,
with the generated line to find it by, so a case can be added for it. Some can't be reached on purpose, such as a
failed allocation.
"""
import collections
import json
import re
import sys

ERROR_LINE = re.compile(r"""errmsg_? *= *['"]|stat_? *= *(ARG|READ|WRITE|ALLOC)_ERR""")


def main(report_path):
    with open(report_path) as f:
        report = json.load(f)

    reached = collections.Counter()
    missed = collections.Counter()
    first_line = {}

    for entry in report["files"]:
        with open(entry["file"]) as f:
            source = f.read().split("\n")
        counts = collections.defaultdict(int)
        for line in entry["lines"]:
            counts[line["line_number"]] += line["count"]
        for number, count in counts.items():
            text = source[number - 1].strip()
            if not ERROR_LINE.search(text):
                continue
            key = (entry["file"], text)
            if count:
                reached[key] += 1
            else:
                missed[key] += 1
                first_line.setdefault(key, number)

    never = sorted(key for key in missed if not reached[key])
    print("## Error checks no test reaches\n")
    if not never:
        print("Every error check is reached by at least one test.")
        return
    print(f"{len(never)} lines, each repeated for some or all kinds:\n")
    print("| Where | Copies | Line |")
    print("|---|---|---|")
    for key in never:
        text = key[1].replace("|", "\\|")
        print(f"| `{key[0]}:{first_line[key]}` | {missed[key]} | `{text}` |")


if __name__ == "__main__":
    main(sys.argv[1])
