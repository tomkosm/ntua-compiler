#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
GRACEC="$SCRIPT_DIR/../bin/gracec"

LIB_DIR="$SCRIPT_DIR/../lib"

TMP_DIR=$(mktemp -d)

if [ ! -f "$GRACEC" ]; then
    echo "Error: gracec not found. Please build the project first."
    rm -r "$TMP_DIR"
    exit 1
fi

if [ -z "$1" ]; then
    echo "Usage: $0 [source file]"
    rm -r "$TMP_DIR"
    exit 1
fi

$GRACEC < "$1" > "$TMP_DIR/arxeio.ll" 2> /dev/null
llc -o "$TMP_DIR/arxeio.s" "$TMP_DIR/arxeio.ll"
clang -o "a.out" "$TMP_DIR/arxeio.s" "$LIB_DIR/libgrace.a" -no-pie
rm -r "$TMP_DIR"

