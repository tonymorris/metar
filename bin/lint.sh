#!/usr/bin/env bash
#
# Run hlint and fourmolu checks across all Haskell source directories.
#
# Usage:
#   bin/lint.sh          Check only (fails on issues, suitable for CI)
#   bin/lint.sh --fix    Apply hlint suggestions and reformat with fourmolu
#
# Tool resolution (for both hlint and fourmolu):
#   1. Use HLINT_EXE / FOURMOLU_EXE environment variable if set
#   2. Otherwise use hlint / fourmolu from PATH
#   3. If not found, install via cabal
#
# Directories checked: src, src-exe, test.
#
# Examples:
#   bin/lint.sh
#   bin/lint.sh --fix
#   HLINT_EXE=/opt/hlint/bin/hlint bin/lint.sh
#   FOURMOLU_EXE="$HOME/.local/bin/fourmolu" bin/lint.sh --fix

set -euo pipefail

unset CDPATH
script_dir=$(dirname "$0")
root_dir=$(cd "${script_dir}/.." && pwd)

fix_mode=false
if [ "${1:-}" = "--fix" ]; then
  fix_mode=true
fi

if [ -n "${HLINT_EXE:-}" ]; then
  hlint_cmd="${HLINT_EXE}"
elif command -v hlint >/dev/null 2>&1; then
  hlint_cmd="hlint"
else
  echo "hlint not found, installing..."
  cabal install hlint --install-method=copy --overwrite-policy=always
  hlint_cmd="hlint"
fi

if [ -n "${FOURMOLU_EXE:-}" ]; then
  fourmolu_cmd="${FOURMOLU_EXE}"
elif command -v fourmolu >/dev/null 2>&1; then
  fourmolu_cmd="fourmolu"
else
  echo "fourmolu not found, installing..."
  cabal install fourmolu --install-method=copy --overwrite-policy=always
  fourmolu_cmd="fourmolu"
fi

dirs=(src src-exe test)

fail=0

echo "=== hlint ==="
if [ "${fix_mode}" = true ]; then
  if ! command -v refactor >/dev/null 2>&1; then
    echo "apply-refact (refactor) not found, installing..."
    cabal install apply-refact --install-method=copy --overwrite-policy=always
  fi
  for d in "${dirs[@]}"; do
    target="${root_dir}/${d}"
    if [ -d "${target}" ]; then
      find "${target}" -name '*.hs' -type f | while read -r f; do
        "${hlint_cmd}" "${f}" --refactor --refactor-options="--inplace" || fail=1
      done
    fi
  done
else
  for d in "${dirs[@]}"; do
    target="${root_dir}/${d}"
    if [ -d "${target}" ]; then
      "${hlint_cmd}" "${target}" || fail=1
    fi
  done
fi

if [ "${fix_mode}" = true ]; then
  fourmolu_mode="inplace"
else
  fourmolu_mode="check"
fi

echo "=== fourmolu ==="
for d in "${dirs[@]}"; do
  target="${root_dir}/${d}"
  if [ -d "${target}" ]; then
    files=$(find "${target}" -name '*.hs' -type f)
    if [ -n "${files}" ]; then
      if ! echo "${files}" | xargs "${fourmolu_cmd}" --mode "${fourmolu_mode}"; then
        fail=1
      fi
    fi
  fi
done

if [ "${fail}" -ne 0 ]; then
  echo "Lint checks failed." >&2
  exit 1
fi

if [ "${fix_mode}" = true ]; then
  echo "All fixes applied."
else
  echo "All lint checks passed."
fi
