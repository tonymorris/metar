#!/usr/bin/env bash
#
# Run doctests against the library.
#
# Usage:
#   bin/doctest.sh
#
# Tool resolution:
#   1. Use DOCTEST_EXE if set
#   2. Otherwise use doctest from PATH
#   3. If not found, install via cabal

set -euo pipefail

unset CDPATH
script_dir=$(cd "$(dirname "$0")" && pwd)
root_dir=$(cd "${script_dir}/.." && pwd)
cd "${root_dir}"

if [ -n "${DOCTEST_EXE:-}" ]; then
  doctest_cmd="${DOCTEST_EXE}"
elif command -v doctest >/dev/null 2>&1; then
  doctest_cmd="doctest"
else
  echo "doctest not found, installing..."
  cabal install doctest --install-method=copy --overwrite-policy=always
  doctest_cmd="doctest"
fi

# doctest needs the package DB. cabal writes a .ghc.environment.* file that
# GHC (and thus doctest) picks up from the CWD when we ask it to.
cabal build --write-ghc-environment-files=always all >/dev/null

"${doctest_cmd}" \
  -isrc \
  -XLambdaCase \
  -XOverloadedStrings \
  -XCPP \
  -XTemplateHaskell \
  -XFlexibleInstances \
  -XFunctionalDependencies \
  -XTypeFamilies \
  -XTypeOperators \
  src/Data/Aviation/Metar/Cache.hs \
  src/Data/Aviation/Metar/METARResult.hs \
  src/Data/Aviation/Metar/METARResultT.hs \
  src/Data/Aviation/Metar.hs
