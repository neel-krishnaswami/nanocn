(** Solver answer classification — see solverOutput.mli. *)

type answer = Sat | Unsat | Unknown | Error of string

let print fmt = function
  | Sat -> Format.pp_print_string fmt "sat"
  | Unsat -> Format.pp_print_string fmt "unsat"
  | Unknown -> Format.pp_print_string fmt "unknown"
  | Error msg -> Format.fprintf fmt "error: %s" msg

let classify_sexp sexp =
  match SmtSexp.shape sexp with
  | SmtSexp.Atom (SmtAtom.Symbol "sat") -> Sat
  | SmtSexp.Atom (SmtAtom.Symbol "unsat") -> Unsat
  | SmtSexp.Atom (SmtAtom.Symbol "unknown") -> Unknown
  | SmtSexp.List items ->
    (match List.map SmtSexp.shape items with
     | SmtSexp.Atom (SmtAtom.Symbol "error") :: rest ->
       let msg =
         List.map (function
           | SmtSexp.Atom (SmtAtom.String s) -> s
           | other ->
             let sexp = SmtSexp.mk (object method loc = SourcePos.dummy end) other in
             SmtSexp.to_string sexp)
           rest
         |> String.concat " "
       in
       Error msg
     | _ -> Error ("unparsed solver output: " ^ SmtSexp.to_string sexp))
  | _ -> Error ("unparsed solver output: " ^ SmtSexp.to_string sexp)

let classify_output text ~file =
  match SmtParse.parse_sexps text ~file with
  | Ok sexps -> Ok (List.map classify_sexp sexps)
  | Error msg -> Error ("solver output parse error: " ^ msg)

module Test = struct
  let test = []
end
