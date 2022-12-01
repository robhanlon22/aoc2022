#!/usr/bin/env bash

set -euo pipefail

status=0

for file in "$@"; do
    hlint "$file" --color --refactor --refactor-options="--inplace" || {
        status=1
        true
    }
done

exit $status
