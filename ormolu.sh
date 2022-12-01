#!/usr/bin/env bash

set -eufo pipefail

ormolu -m check "$@" || ormolu -i "$@"
