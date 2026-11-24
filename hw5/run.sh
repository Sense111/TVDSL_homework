#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "$0")"
N="${NUSMV:-NuSMV}"
mkdir -p logs

"$N" peterson.smv > logs/peterson.log 2>&1

"$N" -bmc -bmc_length 12 peterson.smv > logs/peterson_bmc12.log 2>&1

"$N" -coi -dynamic peterson.smv > logs/peterson_coi_dynamic.log 2>&1

printf 'read_model -i peterson.smv\nflatten_hierarchy\nencode_variables\nbuild_model\ncompute_reachable\nprint_reachable_states -v\nquit\n' \
  | "$N" -int > logs/reachable.log 2>&1

grep -h -E '^-- specification|reachable states' logs/peterson.log logs/reachable.log
