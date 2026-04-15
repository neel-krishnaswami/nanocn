#!/bin/bash
# Classify every error state in lib/parser.messages.
#
# Usage:
#   scripts/classify_states.sh              # all states, one per line
#   scripts/classify_states.sh --form iter  # only states belonging to form "iter"
#   scripts/classify_states.sh --state 148  # only state 148
#   scripts/classify_states.sh --placeholders-only
#   scripts/classify_states.sh --summary    # per-form counts
#
# Output columns (tab-separated):
#   state  kind  form  has_placeholder  suffix
#
# "kind" is "surface" or "refined" (refined means the items mention
# crt_expr/rdecl/rfun/rpat/lpf/rpf/pf_* or similar).
#
# Run from the repo root.

set -e
MESSAGES="${MESSAGES_FILE:-lib/parser.messages}"
[ -f "$MESSAGES" ] || { echo "missing $MESSAGES (run from repo root)"; exit 1; }

FILTER_FORM=""
FILTER_STATE=""
PLACEHOLDERS_ONLY=0
SUMMARY=0

while [ $# -gt 0 ]; do
  case "$1" in
    --form) FILTER_FORM="$2"; shift 2 ;;
    --state) FILTER_STATE="$2"; shift 2 ;;
    --placeholders-only) PLACEHOLDERS_ONLY=1; shift ;;
    --summary) SUMMARY=1; shift ;;
    --help|-h)
      sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
      exit 0 ;;
    *) echo "unknown flag: $1"; exit 1 ;;
  esac
done

awk -v filter_form="$FILTER_FORM" -v filter_state="$FILTER_STATE" \
    -v phonly="$PLACEHOLDERS_ONLY" -v summary="$SUMMARY" '
function classify_form(items,    f) {
  # Surface declarations
  if (items ~ /decl -> FUN /) return "fun-decl"
  if (items ~ /decl -> SORT /) return "sort-decl"
  if (items ~ /decl -> TYPE /) return "type-decl"
  if (items ~ /prog_eof -> list\(decl\) MAIN/) return "main-entry"
  if (items ~ /branch -> pat/ && items !~ /case_branch/) return "fun-branch"
  if (items ~ /case_branch -> pat/) return "case-branch"
  if (items ~ /ctor_decl -> LABEL/) return "sort-ctor-decl"
  if (items ~ /type_ctor_decl -> LABEL/) return "type-ctor-decl"
  # Surface expressions
  if (items ~ /seq_expr -> LET /) return "let"
  if (items ~ /seq_expr -> TAKE /) return "take"
  if (items ~ /seq_expr -> IF /) return "if"
  if (items ~ /seq_expr -> CASE /) return "case"
  if (items ~ /seq_expr -> ITER /) return "iter"
  if (items ~ /seq_expr -> RETURN /) return "return"
  if (items ~ /seq_expr -> FAIL/) return "fail"
  if (items ~ /app_expr -> state_prim /) return "state-prim"
  if (items ~ /app_expr -> NOT_KW/) return "not"
  if (items ~ /simple_expr -> LPAREN/) return "paren-expr"
  if (items ~ /app_expr -> IDENT|app_expr -> LABEL/) return "app-expr"
  if (items ~ /pat -> LABEL|atomic_pat -> LPAREN|atomic_pat -> IDENT|separated_nonempty_list\(COMMA,pat\)/) return "pattern"
  if (items ~ /sort -> PRED|sort -> PTR|atomic_sort -> /) return "sort-form"
  if (items ~ /(add_expr|mul_expr|cmp_expr|and_expr|or_expr|eq_expr)/) return "expr-precedence"
  if (items ~ /expr -> seq_expr/) return "expr-annot"
  if (items ~ /list\(decl\)/) return "top-level"
  if (items ~ /separated_nonempty_list\(BAR,branch\)|separated_nonempty_list\(BAR,case_branch\)/) return "branch-list"
  if (items ~ /separated_nonempty_list\(COMMA,IDENT\)/) return "type-params"
  if (items ~ /separated_nonempty_list\(COMMA,expr\)/) return "expr-list"
  if (items ~ /separated_nonempty_list\(COMMA,sort\)|separated_nonempty_list\(STAR,sort\)/) return "sort-list"
  if (items ~ /separated_nonempty_list\(BAR,ctor_decl\)|separated_nonempty_list\(BAR,type_ctor_decl\)/) return "ctor-list"
  # Refined declarations
  if (items ~ /rprog_eof -> list\(rdecl\) MAIN/) return "rmain-entry"
  if (items ~ /rdecl -> RFUN/) return "rfun-decl"
  if (items ~ /rdecl -> FUN/) return "rfun-in-rdecl"
  if (items ~ /rdecl -> SORT/) return "rsort-decl"
  if (items ~ /rdecl -> TYPE/) return "rtype-decl"
  # Refined signature internals
  if (items ~ /pf_domain_entry/) return "pf-domain-entry"
  if (items ~ /pf_domain /) return "pf-domain"
  if (items ~ /pf_entry/) return "pf-entry"
  if (items ~ /pf_sort/) return "pf-sort"
  # Refined terms
  if (items ~ /crt_seq_expr -> LET CORE/) return "let-core"
  if (items ~ /crt_seq_expr -> LET LOG/) return "let-log"
  if (items ~ /crt_seq_expr -> LET RES/) return "let-res"
  if (items ~ /crt_seq_expr -> LET /) return "let-crt"
  if (items ~ /crt_seq_expr -> CASE /) return "case-crt"
  if (items ~ /crt_case_branch/) return "crt-case-branch"
  if (items ~ /crt_seq_expr -> IF /) return "if-crt"
  if (items ~ /crt_seq_expr -> ITER /) return "iter-crt"
  if (items ~ /crt_seq_expr -> OPEN_TAKE/) return "open-take"
  if (items ~ /rpf_atom_expr -> MAKE_TAKE/) return "make-take"
  if (items ~ /rpf_atom_expr -> MAKE_RET/) return "make-ret"
  if (items ~ /lpf_atom_expr -> UNFOLD/) return "unfold"
  if (items ~ /lpf_atom_expr -> OPEN_RET/) return "open-ret"
  if (items ~ /crt_app_expr -> /) return "crt-app-expr"
  if (items ~ /crt_spine_expr -> /) return "crt-spine"
  if (items ~ /spine_arg/) return "spine-arg"
  if (items ~ /rpf_expr/ || items ~ /rpf_atom_expr/) return "rpf-expr"
  if (items ~ /lpf_expr/ || items ~ /lpf_atom_expr/) return "lpf-expr"
  if (items ~ /rpat_elem/) return "rpat-elem"
  if (items ~ /rpat /) return "rpat"
  if (items ~ /crt_seq_expr/) return "crt-seq-expr"
  if (items ~ /list\(rdecl\)/) return "rtop-level"
  return "other"
}

function is_refined(items) {
  return (items ~ /crt_expr|crt_seq_expr|crt_case|rdecl|rfun\b|rpat|rtake|lpf|rpf|pf_sort|pf_domain|rprog|spine_arg|pf_entry|pf_domain_entry|rpat_elem/)
}

/^## Ends in an error in state:/ {
  if (state != "") flush()
  s = $0; sub(/.*state: /,"",s); sub(/\..*/,"",s); state=s
  items=""; suffix=""; msg=""; mode="items"
  next
}
/^## The known suffix of the stack is as follows:/ {
  mode="suffix"; next
}
mode=="suffix" && /^##/ {
  sfx=$0; sub(/^## */,"",sfx); if (suffix=="") suffix=sfx; mode="tail"; next
}
mode=="items" && /^##/ { items = items $0 " " }
/^$/ && state != "" && mode != "msg" { mode="msg"; next }
mode=="msg" && /./ { if (msg=="") msg=$0; mode="done" }
END { if (state != "") flush() }

function flush(    itemstxt, form, kind, placeholder) {
  itemstxt = items
  gsub(/\[[^\]]*\]/, "", itemstxt)
  form = classify_form(itemstxt)
  kind = is_refined(itemstxt) ? "refined" : "surface"
  placeholder = (msg ~ /^PLACEHOLDER/) ? "PH" : "ok"
  if (filter_state != "" && state != filter_state) { state=""; return }
  if (filter_form != "" && form != filter_form) { state=""; return }
  if (phonly == 1 && placeholder != "PH") { state=""; return }
  if (summary == 1) {
    counts[form]++
    if (placeholder == "PH") phcounts[form]++
  } else {
    printf "%s\t%s\t%s\t%s\t%s\n", state, kind, form, placeholder, suffix
  }
  state=""
}
END {
  if (summary == 1) {
    n = asorti(counts, sorted)
    printf "%-20s %6s %6s\n", "form", "total", "PH"
    for (i=1; i<=n; i++) {
      f = sorted[i]
      printf "%-20s %6d %6d\n", f, counts[f], (f in phcounts ? phcounts[f] : 0)
    }
  }
}
' "$MESSAGES"
