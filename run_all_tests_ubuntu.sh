#!/usr/bin/env bash

set -uo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

LOG_FILE="output.log"
TESTS=(
  ataxno
  gemmno
  k2mmno
  k3mmno
  fftno
  mno
  kmpno
  nwno
  spmvno
)

if ! command -v sbt >/dev/null 2>&1; then
  printf 'Error: sbt was not found in PATH.\n' >&2
  exit 127
fi

failures=()

for test_name in "${TESTS[@]}"; do
  printf '\n========== Running %s ==========\n' "$test_name"

  sbt -mem 12384 -batch -no-colors \
    "runMain mycgratemporal.Main $test_name" \
    >"$LOG_FILE" 2>&1
  test_status=$?

  if (( test_status != 0 )); then
    printf 'Test process exited with status %d. Parsing its log before continuing.\n' "$test_status" >&2
    failures+=("$test_name:test:$test_status")
  fi

  printf '%s\n' "---------- Parsing $LOG_FILE ----------"
  sbt -mem 12384 -batch -no-colors \
    "runMain OutParse.Main $LOG_FILE"
  parse_status=$?

  if (( parse_status != 0 )); then
    printf 'Log parser exited with status %d.\n' "$parse_status" >&2
    failures+=("$test_name:parser:$parse_status")
  fi
done

printf '\n========== Test sequence finished ==========\n'

if (( ${#failures[@]} > 0 )); then
  printf 'Failures:\n' >&2
  printf '  %s\n' "${failures[@]}" >&2
  exit 1
fi

printf 'All test processes and log parser runs completed successfully.\n'
