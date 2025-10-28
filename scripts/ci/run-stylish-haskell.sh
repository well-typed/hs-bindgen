#!/usr/bin/env bash

set -euo pipefail

# Enable globstar for ** pattern matching
shopt -s globstar

# ==============================================================================
# Constants
# ==============================================================================

readonly SCRIPT_NAME="$(basename "$0")"
readonly STYLISH_CONFIG=".stylish-haskell.yaml"
readonly IGNORE_FILE="scripts/ci/check-stylish-ignore"
readonly HASKELL_EXTENSION="hs"

# Stylish-haskell arguments
readonly STYLISH_ARGS="-c ${STYLISH_CONFIG} -i"

# Exit codes
readonly EXIT_SUCCESS=0
readonly EXIT_FORMATTING_CHANGES=1
readonly EXIT_INVALID_OPTION=2

# ==============================================================================
# Global Variables
# ==============================================================================

export LC_ALL=C.UTF-8

# Default values for command-line options
check_mode="all"
use_git_diff=true

# ==============================================================================
# Usage and Help
# ==============================================================================

usage() {
    cat <<EOF
Usage: ${SCRIPT_NAME} [-uch]

Check Haskell files with 'stylish-haskell'; by default check all files.

Options:
  -u    Only check uncommitted files
  -c    Only check committed files in HEAD
  -g    Don't show the diff with git
  -h    Show this help message

Examples:
  ${SCRIPT_NAME}          # Check all Haskell files
  ${SCRIPT_NAME} -u       # Check only uncommitted files
  ${SCRIPT_NAME} -c -g    # Check committed files without showing git diff

EOF
    exit "${EXIT_SUCCESS}"
}

# ==============================================================================
# Utility Functions
# ==============================================================================

# Log an informational message
log_info() {
    echo "$@"
}

# Log a warning message
log_warning() {
    echo "Warning: $*" >&2
}

# Log an error message
log_error() {
    echo "Error: $*" >&2
}

# Check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# ==============================================================================
# File Discovery Functions
# ==============================================================================

# Find all Haskell files in the project
find_all_haskell_files() {
    git ls-files --exclude-standard --no-deleted --deduplicate "*.${HASKELL_EXTENSION}"
}

# Get uncommitted Haskell files from git
find_uncommitted_files() {
    git diff --name-only --diff-filter=ACMR | grep "\.${HASKELL_EXTENSION}$" || true
}

# Get committed Haskell files from the last commit
find_committed_files() {
    # Check if HEAD~1 exists (handles shallow clones and single commits)
    if git rev-parse --verify HEAD~1 >/dev/null 2>&1; then
        git diff --name-only HEAD~1 HEAD --diff-filter=ACMR | \
            grep "\.${HASKELL_EXTENSION}$" || true
    else
        log_warning "HEAD~1 not available, checking all files instead"
        find_all_haskell_files
    fi
}

# Get the list of files to check based on the selected mode
get_files_to_check() {
    case "${check_mode}" in
        uncommitted)
            find_uncommitted_files
            ;;
        committed)
            find_committed_files
            ;;
        all)
            find_all_haskell_files
            ;;
        *)
            log_error "Invalid check mode: ${check_mode}"
            exit "${EXIT_INVALID_OPTION}"
            ;;
    esac
}

# ==============================================================================
# File Filtering Functions
# ==============================================================================

# Check if file contains CPP directives in the imports section
# Returns 0 if CPP found in imports, 1 otherwise
has_cpp_in_imports() {
    local file="$1"

    [[ -f "${file}" ]] || return 1

    # Look for CPP directives in the imports section
    # This is a heuristic: check for #if, #ifdef, #ifndef, #else, #endif
    awk '
        /^import / || /^{-# LANGUAGE/ || /^module / {
            in_imports = 1
        }
        /^[a-zA-Z]/ && !/^import / && !/^{-# LANGUAGE/ && !/^module / && in_imports {
            in_imports = 0
        }
        in_imports && /^#(if|ifdef|ifndef|else|endif)/ {
            found_cpp = 1
            exit
        }
        END {
            exit (found_cpp ? 0 : 1)
        }
    ' "${file}"
}

# Check if file matches any pattern in the ignore file
matches_ignore_pattern() {
    local file="$1"
    local pattern
    local normalized_file

    [[ -f "${IGNORE_FILE}" ]] || return 1

    # Normalize file path (remove leading ./)
    normalized_file="${file#./}"

    # Read ignore patterns (skip comments and empty lines)
    while IFS= read -r pattern; do
        # Skip comments and empty lines
        [[ "${pattern}" =~ ^[[:space:]]*# ]] && continue
        [[ -z "${pattern}" ]] && continue

        # Check if file matches the pattern (using shell pattern matching)
        if [[ "${normalized_file}" == ${pattern} ]]; then
            return 0  # Match found
        fi
    done < "${IGNORE_FILE}"

    return 1  # No match
}

# Determine if a file should be ignored
should_ignore_file() {
    local file="$1"
    local reason=""

    # Check for CPP in imports
    if has_cpp_in_imports "${file}"; then
        reason="CPP in imports"
    # Check ignore patterns
    elif matches_ignore_pattern "${file}"; then
        reason="matched ignore pattern"
    else
        return 1  # Should not ignore
    fi

    log_info "Ignoring ${file} (${reason})"
    return 0
}

# ==============================================================================
# Processing Functions
# ==============================================================================

# Process a single file with stylish-haskell
process_file() {
    local file="$1"

    if [[ ! -f "${file}" ]]; then
        log_warning "File not found: ${file}"
        return 1
    fi

    if should_ignore_file "${file}"; then
        return 0
    fi

    log_info "Checking: ${file}"
    # shellcheck disable=SC2086
    stylish-haskell ${STYLISH_ARGS} "${file}"
}

# Process all files in the list
process_files() {
    local files="$1"
    local file

    if [[ -z "${files}" ]]; then
        log_info "No Haskell files to check."
        return 0
    fi

    log_info "Checking Haskell files with stylish-haskell..."

    while IFS= read -r file; do
        process_file "${file}"
    done <<< "${files}"
}

# Check for formatting changes using git diff
check_formatting_changes() {
    if [[ "${use_git_diff}" == true ]]; then
        log_info "Checking for formatting changes..."

        # We only check differences in Haskell source code, because other files
        # may change in CI (e.g., `cabal.project` files).
        if ! git diff --exit-code --quiet *.hs; then
            log_info ""
            log_info "Git diff after stylish-haskell:"
            git diff *.hs
            log_error "stylish-haskell made changes. Please commit them or run this script to fix formatting."
            return "${EXIT_FORMATTING_CHANGES}"
        fi
    fi

    return "${EXIT_SUCCESS}"
}

# ==============================================================================
# Main Program
# ==============================================================================

# Parse command-line options
parse_options() {
    local opt

    while getopts "uchg" opt; do
        case "${opt}" in
            u)
                check_mode="uncommitted"
                ;;
            c)
                check_mode="committed"
                ;;
            g)
                use_git_diff=false
                ;;
            h)
                usage
                ;;
            \?)
                log_error "Invalid option: -${OPTARG}"
                usage
                ;;
        esac
    done
}

# Main execution
main() {
    parse_options "$@"

    # Verify stylish-haskell is available
    if ! command_exists stylish-haskell; then
        log_error "stylish-haskell not found. Please install it first."
        exit "${EXIT_INVALID_OPTION}"
    fi

    # Get and process files
    local files
    files="$(get_files_to_check)"

    process_files "${files}"

    # Check for formatting changes
    if ! check_formatting_changes; then
        exit "${EXIT_FORMATTING_CHANGES}"
    fi

    log_info "All files are correctly formatted!"
    exit "${EXIT_SUCCESS}"
}

# Run main function with all arguments
main "$@"
