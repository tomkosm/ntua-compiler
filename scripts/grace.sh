#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
GRACEC="$SCRIPT_DIR/../bin/gracec"
LIB_DIR="$SCRIPT_DIR/../lib"

# Function to create a temporary file and return its path
create_temp_file() {
    mktemp "/tmp/gracec.XXXXXX"
}

# Initialize variables
MODE="default"
OPTIMIZE=""

source_file_encountered=false

# Iterate over all arguments
for arg in "$@"; do
    if [ "$source_file_encountered" = false ]; then
        # Handle options
        case "$arg" in
            -f) MODE="final";;
            -i) MODE="intermediate";;
            -O) OPTIMIZE="-O";;
            *) SOURCE_FILE="$arg" # This line marks the end of options
        esac
    fi
done

echo $OPTIMIZE
# Handle different modes
case "$MODE" in
    final)
        # Compile from standard input to final code using clang and output to standard output
        IMM_FILE=$(create_temp_file)
        ASM_FILE=$(create_temp_file)
        EXEC_FILE=$(create_temp_file)

        $GRACEC $OPTIMIZE > "$IMM_FILE" 2>/dev/null || { echo "Failed to compile with gracec" >&2; cat "$IMM_FILE"; rm "$IMM_FILE"; exit 1; }
        llc -o "$ASM_FILE" "$IMM_FILE" || { echo "LLVM llc failed" >&2; rm "$IMM_FILE" "$ASM_FILE"; exit 1; }
        clang -o "$EXEC_FILE" "$ASM_FILE" "$LIB_DIR/libgrace.a" -no-pie || { echo "Clang compilation failed" >&2; rm "$IMM_FILE" "$ASM_FILE" "$EXEC_FILE"; exit 1; }
        cat "$EXEC_FILE"
        rm "$IMM_FILE" "$ASM_FILE" "$EXEC_FILE"
        ;;

    intermediate)
        # Compile from standard input to intermediate code using llc and output to standard output
        IMM_FILE=$(create_temp_file)
        $GRACEC $OPTIMIZE > "$IMM_FILE" 2>/dev/null || { echo "Failed to compile with gracec" >&2;cat "$IMM_FILE"; rm "$IMM_FILE"; exit 1; }
        llc "$IMM_FILE" || { echo "LLVM llc failed" >&2; rm "$IMM_FILE"; exit 1; }
        rm "$IMM_FILE"
        ;;

    *)
        # Default behavior: Require a source file
        if [ $# -eq 0 ]; then
            echo "No source file provided. Usage: $0 [options] <source_file>" >&2
            exit 1
        fi
#        SOURCE_FILE="$1"
        DIR_PATH=$(dirname "$SOURCE_FILE")
        FILENAME=$(basename -- "$SOURCE_FILE")
        FILENAME="${FILENAME%.*}"
        IMM_FILE="${DIR_PATH}/${FILENAME}.imm"
        ASM_FILE="${DIR_PATH}/${FILENAME}.asm"

        $GRACEC $OPTIMIZE < "$SOURCE_FILE" > "$IMM_FILE" 2>/dev/null || { echo "Failed to compile with gracec" >&2; cat "$IMM_FILE"; rm "$IMM_FILE"; exit 1; }
        llc -o "$ASM_FILE" "$IMM_FILE" || { echo "LLVM llc failed" >&2; exit 1; }
        clang -o "${DIR_PATH}/${FILENAME}.out" "$ASM_FILE" "$LIB_DIR/libgrace.a" -no-pie || { echo "Clang compilation failed" >&2; exit 1; }
        echo "Generated ${IMM_FILE} and ${ASM_FILE}"
        ;;
esac
