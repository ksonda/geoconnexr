#!/bin/sh
set -eu

if [ "${GEOCONNEXR_RUN_LIVE:-}" != "true" ]; then
  echo "Set GEOCONNEXR_RUN_LIVE=true to run bounded live checks." >&2
  exit 2
fi

command -v curl >/dev/null
command -v jq >/dev/null

tmp_dir="$(mktemp -d)"
trap 'rm -rf "$tmp_dir"' EXIT INT TERM

query_file="$tmp_dir/mainstem.rq"
response_file="$tmp_dir/mainstem.json"

sed \
  -e 's#{{mainstem_uri}}#<https://geoconnex.us/ref/mainstems/1622734>#g' \
  -e 's/{{limit}}/10/g' \
  -e 's/{{offset}}/0/g' \
  inst/queries/sites_on_mainstem.rq >"$query_file"

curl -sS \
  --proto '=https' \
  --connect-timeout 10 \
  --max-time 30 \
  --max-filesize 1048576 \
  --retry 1 \
  --request POST \
  --header 'Content-Type: application/sparql-query' \
  --header 'Accept: application/sparql-results+json' \
  --data-binary "@$query_file" \
  --output "$response_file" \
  https://graph.geoconnex.us/

site_count="$(jq '[.results.bindings[].site.value] | unique | length' "$response_file")"
printf 'mainstem_1622734_distinct_sites=%s (review evidence: 4)\n' "$site_count"

curl -sS \
  --proto '=https' \
  --connect-timeout 10 \
  --max-time 30 \
  --max-filesize 2097152 \
  --retry 1 \
  --get \
  --data-urlencode 'provider_id=USGS-08332622' \
  --data-urlencode 'limit=2' \
  --data-urlencode 'f=json' \
  'https://reference.geoconnex.us/collections/gages/items' |
  jq '{numberReturned, known_answer: (.features[0] | {id, properties: {provider_id: .properties.provider_id, mainstem_uri: .properties.mainstem_uri, nhdpv2_comid: .properties.nhdpv2_comid}})}'
