#!/usr/bin/env bash
# RALPH loop for Common Lisp projects (SBCL + ASDF + clunit2)
# Drives the GitHub Copilot CLI in a non-interactive loop, checking
# prd.json between iterations for completion.
#
# Usage:
#   ./ralph.sh                          # defaults: copilot, claude-sonnet-4.6, 30 iters
#   ./ralph.sh 10                       # 10 iterations
#   ./ralph.sh --model claude-opus-4.6  # use a different model
#   ./ralph.sh --model gpt-5.2-codex 5 # different model, 5 iterations
#
# Both this script and VS Code Copilot Chat (@ralph agent / ralph-loop prompt)
# drive from the same prd.json and progress.txt — do not run both simultaneously.

set -euo pipefail

# --- Defaults ---
MODEL="claude-sonnet-4.6"
MAX_ITERATIONS=30
PRD_FILE="prd.json"
PROGRESS_FILE="progress.txt"

# BEGIN PROJECT-SPECIFIC
PROJECT_DIR="/workspace"
# END PROJECT-SPECIFIC

# --- Argument parsing ---
while [[ $# -gt 0 ]]; do
  case "$1" in
    --model)
      MODEL="$2"
      shift 2
      ;;
    --help|-h)
      echo "Usage: ./ralph.sh [--model MODEL] [max_iterations]"
      echo ""
      echo "Options:"
      echo "  --model MODEL   AI model (default: claude-sonnet-4.6)"
      echo "  max_iterations  Number of outer-loop iterations (default: 30)"
      echo ""
      echo "Available models: claude-sonnet-4.6, claude-opus-4.6, gpt-5.2-codex, gpt-5.1-codex, etc."
      exit 0
      ;;
    *)
      if [[ "$1" =~ ^[0-9]+$ ]]; then
        MAX_ITERATIONS="$1"
      else
        echo "Unknown argument: $1"
        echo "Usage: ./ralph.sh [--model MODEL] [max_iterations]"
        exit 1
      fi
      shift
      ;;
  esac
done

# --- Verify prerequisites ---
if ! command -v copilot &>/dev/null; then
  echo "Error: 'copilot' CLI not found. Install GitHub Copilot CLI first."
  echo "  See: https://docs.github.com/copilot/how-tos/copilot-cli"
  exit 1
fi

if ! command -v jq &>/dev/null; then
  echo "Error: 'jq' not found. Install it: apt install jq"
  exit 1
fi

if [[ ! -f "${PRD_FILE}" ]]; then
  echo "Error: ${PRD_FILE} not found in current directory."
  exit 1
fi

# --- Prompt for the AI agent ---
PROMPT="You are the RALPH orchestrator for num-utils.

Read prd.json and progress.txt. Pick the highest-priority user story where passes is false.

For that story:
1. Implement the required changes (source in src/, tests in tests/)
2. Run the test suite:
   cd ${PROJECT_DIR} && sbcl --non-interactive \\
     --eval '(push #p\"${PROJECT_DIR}/\" asdf:*central-registry*)' \\
     --eval '(ql:quickload :num-utils :silent t)' \\
     --eval '(ql:quickload :clunit2 :silent t)' \\
     --eval '(asdf:test-system \"num-utils\")'
3. If tests fail, analyze the failure, fix it, and re-run (up to 10 attempts)
4. When green:
   a. Update prd.json: set passes to true for this story, add notes
   b. Append any new learnings to progress.txt
   c. Commit with message: feat(US-XXX): <story title>
5. If capacity remains, pick the next passes:false story and repeat

Follow all conventions in .github/copilot-instructions.md and .github/instructions/common-lisp.instructions.md."

echo "=== RALPH Loop ==="
echo "Model: ${MODEL}"
echo "Max iterations: ${MAX_ITERATIONS}"
echo "PRD: ${PRD_FILE}"
echo ""

for ((i=1; i<=MAX_ITERATIONS; i++)); do
  echo "--- Iteration ${i}/${MAX_ITERATIONS} ---"

  # Check completion status
  REMAINING=$(jq '[.userStories[] | select(.passes == false)] | length' "${PRD_FILE}")
  if [[ "${REMAINING}" -eq 0 ]]; then
    echo ""
    echo "ALL STORIES PASS. RALPH loop complete."
    exit 0
  fi

  NEXT_STORY=$(jq -r '[.userStories[] | select(.passes == false)] | sort_by(.priority) | .[0] | "\(.id): \(.title)"' "${PRD_FILE}")
  echo "Remaining stories: ${REMAINING}"
  echo "Next story: ${NEXT_STORY}"
  echo ""

  # Run the Copilot CLI agent
  copilot -p "${PROMPT}" \
    --yolo \
    --model "${MODEL}" \
    --no-ask-user \
    --add-dir "${PROJECT_DIR}"

  echo ""
  echo "--- Iteration ${i} complete ---"
  echo ""
done

echo "Max iterations (${MAX_ITERATIONS}) reached."
echo "Remaining stories: $(jq '[.userStories[] | select(.passes == false)] | length' "${PRD_FILE}")"
echo "Check prd.json for details."
