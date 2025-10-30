#!/bin/bash
symbol=BTCUSDT
interval=15m
start=$(date -d "2024-01-01T00:00:00Z" +%s000)
end=$(date -d "2025-01-01T00:00:00Z" +%s000)

outfile="btc_15m_2024.json"
echo "[" > $outfile

current=$start
while [ $current -lt $end ]
do
  url="https://api.binance.com/api/v3/klines?symbol=$symbol&interval=$interval&limit=1000&startTime=$current"
  echo "Fetching from $current"
  json=$(curl -s "$url")
  count=$(echo $json | jq length)
  if [ "$count" -eq 0 ]; then break; fi
  # Append to file (remove [ ] to avoid nesting)
  echo $json | jq -c '.[]' >> tmp.json
  # Compute next startTime (last close_time + 1 ms)
  last_close=$(echo $json | jq -r '.[-1][6]')
  current=$((last_close+1))
done

# Merge into single JSON array
jq -s '.' tmp.json >> $outfile
rm tmp.json
echo "]" >> $outfile
