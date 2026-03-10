#!/usr/bin/env bash
# Run ERT tests for the fate emacs config.
# Usage: ./test/run-tests.sh [test-file.el ...]
# With no arguments, runs all test files.
set -euo pipefail

EMACS="${EMACS:-emacs}"
TEST_DIR="$(cd "$(dirname "$0")" && pwd)"

if [ $# -gt 0 ]; then
  test_files=("$@")
else
  test_files=("$TEST_DIR"/test-*.el)
fi

exit_code=0
for f in "${test_files[@]}"; do
  echo "--- Running: $(basename "$f") ---"
  "$EMACS" -Q --batch \
    -l "$f" \
    -f ert-run-tests-batch-and-exit \
    2>&1 || exit_code=1
  echo
done

if [ $exit_code -eq 0 ]; then
  echo "All tests passed."
else
  echo "Some tests FAILED." >&2
fi
exit $exit_code
