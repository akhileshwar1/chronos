#!/usr/bin/env bash
# klines_to_csv_kolkata.sh
# Converts Binance klines JSON -> CSV with columns: date,open,high,low,close,volume
# Date formatted in Asia/Kolkata timezone: "YYYY-MM-DD HH:MM:SS+05:30"
#
# Usage:
#   ./klines_to_csv_kolkata.sh input.json output.csv
#
# Requirements:
#   jq (https://stedolan.github.io/jq/)
#   GNU date (on macOS, install coreutils and use gdate)
set -euo pipefail

if [ $# -ne 2 ]; then
  echo "Usage: $0 <input.json> <output.csv>"
  exit 1
fi

IN="$1"
OUT="$2"

# Choose date command: gdate for macOS if available, otherwise date
DATE_CMD="date"
if command -v gdate >/dev/null 2>&1; then
  DATE_CMD="gdate"
fi

# Ensure jq available
if ! command -v jq >/dev/null 2>&1; then
  echo "Error: jq is required. Install it (apt/get, brew install jq, etc.)." >&2
  exit 1
fi

# Produce intermediary TSV of fields: ms_ts\topen\thigh\tlow\tclose\tvolume
tmp_tsv=$(mktemp)
trap 'rm -f "$tmp_tsv"' EXIT

jq -r '
  def reshape:
    if (.[0] | type) == "array" then
      .
    else
      [ range(0; (length/12)|floor) as $i
        | [ .[12*$i + 0], .[12*$i + 1], .[12*$i + 2], .[12*$i + 3], .[12*$i + 4], .[12*$i + 5],
            .[12*$i + 6], .[12*$i + 7], .[12*$i + 8], .[12*$i + 9], .[12*$i + 10], .[12*$i + 11] ]
      ]
    end;
  reshape[] | [ .[0], .[1], .[2], .[3], .[4], .[5] ] | @tsv
' "$IN" > "$tmp_tsv"

# Write header
printf "date,open,high,low,close,volume\n" > "$OUT"

# Convert each TSV row: ms -> Asia/Kolkata datetime string
# Use TZ=Asia/Kolkata to force timezone
while IFS=$'\t' read -r ms open high low close volume; do
  # ms might be quoted/strings; strip quotes and non-digits
  ms_clean=$(echo "$ms" | sed 's/"//g' | sed 's/[^0-9]*//g')
  if [ -z "$ms_clean" ]; then
    continue
  fi
  # seconds since epoch
  secs=$(( ms_clean / 1000 ))
  # Format date in Asia/Kolkata
  # Note: GNU date format: TZ=Asia/Kolkata date -d "@$secs" "+%Y-%m-%d %H:%M:%S%:z"
  # Use env TZ to ensure output offset +05:30
  formatted_date=$(TZ=Asia/Kolkata $DATE_CMD -d "@$secs" "+%Y-%m-%d %H:%M:%S%:z")
  # write CSV row (no quoting)
  printf "%s,%s,%s,%s,%s,%s\n" "$formatted_date" "$open" "$high" "$low" "$close" "$volume" >> "$OUT"
done < "$tmp_tsv"

echo "Wrote CSV -> $OUT"

