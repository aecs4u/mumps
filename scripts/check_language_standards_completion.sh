#!/usr/bin/env bash
set -euo pipefail

MODE="report"
if [[ "${1:-}" == "--gate" ]]; then
  MODE="gate"
elif [[ "${1:-}" == "--report" || -z "${1:-}" ]]; then
  MODE="report"
else
  echo "Usage: $0 [--report|--gate]" >&2
  exit 2
fi

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "${ROOT_DIR}"

trim() {
  sed 's/^[[:space:]]*//;s/[[:space:]]*$//'
}

count_files() {
  local base="$1"
  shift
  rg --files "${base}" "$@" 2>/dev/null | wc -l | tr -d ' '
}

extract_showconfig_value() {
  local key="$1"
  printf '%s\n' "${SHOWCONFIG_OUTPUT}" | awk -v k="${key}" '
    index($0, k "=") == 1 {
      value = substr($0, length(k) + 2)
      sub(/^[[:space:]]+/, "", value)
      print value
      exit
    }'
}

declare -a FAILURES=()
declare -a CHECKLIST=()

record_check() {
  local check_id="$1"
  local description="$2"
  local status="$3"
  local details="$4"
  CHECKLIST+=("${check_id}|${description}|${status}|${details}")
  if [[ "${status}" == "FAIL" ]]; then
    FAILURES+=("${check_id}: ${description} (${details})")
  fi
}

echo "== Language Standards Completion Audit =="
echo "Mode: ${MODE}"
echo "Repo: ${ROOT_DIR}"
echo

fixed_src_count="$(count_files src -g '*.F')"
fixed_template_count="$(count_files src/templates -g '*.F.in')"
free_src_count="$(count_files src -g '*.f90' -g '*.F90')"
c_source_count="$(rg --files -g '*.c' 2>/dev/null | wc -l | tr -d ' ')"
nof2003_refs="$(rg -n 'MUMPS_NOF2003' src src/templates 2>/dev/null | wc -l | tr -d ' ')"

SHOWCONFIG_OUTPUT="$(make --no-print-directory showconfig 2>/dev/null || true)"
CONFIG_AVAILABLE=0
if [[ -z "${SHOWCONFIG_OUTPUT}" ]]; then
  record_check "CFG-001" "Makefile showconfig is readable" "FAIL" "make showconfig produced no output"
  C_STD_MODE=""
  C_STD_FLAG=""
  FORTRAN_FIXED_STD_MODE=""
  FORTRAN_FIXED_STD_FLAG=""
  FORTRAN_FREE_STD_MODE=""
  FORTRAN_FREE_STD_FLAG=""
else
  CONFIG_AVAILABLE=1
  record_check "CFG-001" "Makefile showconfig is readable" "PASS" "output captured"
  C_STD_MODE="$(extract_showconfig_value C_STD_MODE | trim)"
  C_STD_FLAG="$(extract_showconfig_value C_STD_FLAG | trim)"
  FORTRAN_FIXED_STD_MODE="$(extract_showconfig_value FORTRAN_FIXED_STD_MODE | trim)"
  FORTRAN_FIXED_STD_FLAG="$(extract_showconfig_value FORTRAN_FIXED_STD_FLAG | trim)"
  FORTRAN_FREE_STD_MODE="$(extract_showconfig_value FORTRAN_FREE_STD_MODE | trim)"
  FORTRAN_FREE_STD_FLAG="$(extract_showconfig_value FORTRAN_FREE_STD_FLAG | trim)"
fi

if [[ "${CONFIG_AVAILABLE}" -eq 0 ]]; then
  record_check "F23-001" ".F files have explicit standard mode set" "FAIL" "showconfig unavailable"
elif [[ "${FORTRAN_FIXED_STD_MODE}" == "none" || -z "${FORTRAN_FIXED_STD_MODE}" ]]; then
  record_check "F23-001" ".F files have explicit standard mode set" "FAIL" "FORTRAN_FIXED_STD_MODE=${FORTRAN_FIXED_STD_MODE:-<unset>}"
else
  record_check "F23-001" ".F files have explicit standard mode set" "PASS" "FORTRAN_FIXED_STD_MODE=${FORTRAN_FIXED_STD_MODE:-<unset>}"
fi

# F23-002: Accept either modern standard OR legacy mode as valid
if [[ "${CONFIG_AVAILABLE}" -eq 0 ]]; then
  record_check "F23-002" ".F standard is modern (f2023/f2018/f2008) or legacy" "FAIL" "showconfig unavailable"
elif [[ "${FORTRAN_FIXED_STD_FLAG}" == *"-std=f2023"* || "${FORTRAN_FIXED_STD_FLAG}" == *"-std=f2018"* || "${FORTRAN_FIXED_STD_FLAG}" == *"-std=f2008"* || "${FORTRAN_FIXED_STD_FLAG}" == *"-std=legacy"* ]]; then
  record_check "F23-002" ".F standard is modern (f2023/f2018/f2008) or legacy" "PASS" "FORTRAN_FIXED_STD_FLAG=${FORTRAN_FIXED_STD_FLAG:-<unset>}"
else
  record_check "F23-002" ".F standard is modern (f2023/f2018/f2008) or legacy" "FAIL" "FORTRAN_FIXED_STD_FLAG=${FORTRAN_FIXED_STD_FLAG:-<unset>}"
fi

if [[ "${CONFIG_AVAILABLE}" -eq 0 ]]; then
  record_check "F23-004" ".f90 compilation mode uses modern Fortran standard" "FAIL" "showconfig unavailable"
elif [[ "${FORTRAN_FREE_STD_MODE}" == "none" ]]; then
  record_check "F23-004" ".f90 compilation mode uses modern Fortran standard" "FAIL" "FORTRAN_FREE_STD_MODE=${FORTRAN_FREE_STD_MODE:-<unset>}"
else
  record_check "F23-004" ".f90 compilation mode uses modern Fortran standard" "PASS" "FORTRAN_FREE_STD_MODE=${FORTRAN_FREE_STD_MODE:-<unset>}"
fi

if [[ "${CONFIG_AVAILABLE}" -eq 0 ]]; then
  record_check "F23-005" "Resolved .f90 standard is f2023/f2018/f2008" "FAIL" "showconfig unavailable"
elif [[ "${FORTRAN_FREE_STD_FLAG}" == *"-std=f2023"* || "${FORTRAN_FREE_STD_FLAG}" == *"-std=f2018"* || "${FORTRAN_FREE_STD_FLAG}" == *"-std=f2008"* ]]; then
  record_check "F23-005" "Resolved .f90 standard is f2023/f2018/f2008" "PASS" "FORTRAN_FREE_STD_FLAG=${FORTRAN_FREE_STD_FLAG:-<unset>}"
else
  record_check "F23-005" "Resolved .f90 standard is f2023/f2018/f2008" "FAIL" "FORTRAN_FREE_STD_FLAG=${FORTRAN_FREE_STD_FLAG:-<unset>}"
fi

if [[ "${CONFIG_AVAILABLE}" -eq 0 ]]; then
  record_check "C23-001" "C compilation mode is strict gnu23" "FAIL" "showconfig unavailable"
elif [[ "${C_STD_MODE}" == "gnu23" ]]; then
  record_check "C23-001" "C compilation mode is strict gnu23" "PASS" "C_STD_MODE=${C_STD_MODE:-<unset>}"
else
  record_check "C23-001" "C compilation mode is strict gnu23" "FAIL" "C_STD_MODE=${C_STD_MODE:-<unset>}"
fi

if [[ "${CONFIG_AVAILABLE}" -eq 0 ]]; then
  record_check "C23-002" "Resolved C flag is -std=gnu23" "FAIL" "showconfig unavailable"
elif [[ "${C_STD_FLAG}" == "-std=gnu23" ]]; then
  record_check "C23-002" "Resolved C flag is -std=gnu23" "PASS" "C_STD_FLAG=${C_STD_FLAG:-<unset>}"
else
  record_check "C23-002" "Resolved C flag is -std=gnu23" "FAIL" "C_STD_FLAG=${C_STD_FLAG:-<unset>}"
fi

echo "Counts:"
echo "  C sources (*.c): ${c_source_count}"
echo "  Fortran free (*.f90/*.F90): ${free_src_count}"
echo "  Fortran fixed (*.F): ${fixed_src_count}"
echo "  Fixed-form templates (*.F.in): ${fixed_template_count}"
echo "  MUMPS_NOF2003 refs: ${nof2003_refs}"
echo

echo "Checklist:"
for item in "${CHECKLIST[@]}"; do
  IFS='|' read -r check_id description status details <<<"${item}"
  printf '  [%s] %-8s %s (%s)\n' "${check_id}" "${status}" "${description}" "${details}"
done
echo

if [[ "${#FAILURES[@]}" -gt 0 ]]; then
  echo "Summary: ${#FAILURES[@]} check(s) failed."
  if [[ "${MODE}" == "gate" ]]; then
    echo "Fail-fast gate: FAIL"
    exit 1
  else
    echo "Report mode: non-blocking (exit 0)."
    exit 0
  fi
fi

echo "Summary: all checks passed."
if [[ "${MODE}" == "gate" ]]; then
  echo "Fail-fast gate: PASS"
fi
