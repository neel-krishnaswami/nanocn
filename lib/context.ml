type binding =
  | Term of Var.t * Sort.sort * Effect.t
  | TVar of Tvar.t * Kind.t
  | Unknown of Var.t

type t = binding list

let empty = []

let extend x s eff ctx = Term (x, s, eff) :: ctx

let extend_unknown x ctx = Unknown x :: ctx

let extend_or_unknown x sort_result eff ctx =
  match sort_result with
  | Ok s -> extend x s eff ctx
  | Error _ -> extend_unknown x ctx

let extend_tvar a kind ctx = TVar (a, kind) :: ctx

let rec lookup x = function
  | [] -> Error (Error.unbound_var x)
  | Term (y, s, eff) :: rest ->
    if Var.compare x y = 0 then Ok (s, eff)
    else lookup x rest
  | Unknown y :: rest ->
    if Var.compare x y = 0 then Error (Error.unknown_var_type ~var:y)
    else lookup x rest
  | TVar _ :: rest -> lookup x rest

let rec lookup_tvar a = function
  | [] -> None
  | TVar (b, kind) :: rest ->
    if Tvar.compare a b = 0 then Some kind
    else lookup_tvar a rest
  | Term _ :: rest | Unknown _ :: rest -> lookup_tvar a rest

let to_list ctx = ctx

let extend_list bindings ctx =
  List.fold_left (fun acc (x, s, eff) -> Term (x, s, eff) :: acc) ctx (List.rev bindings)

let print_gen pp_var fmt ctx =
  let pp_entry fmt = function
    | Term (x, s, eff) ->
      Format.fprintf fmt "@[%a : %a [%a]@]" pp_var x Sort.print s Effect.print eff
    | Unknown x ->
      Format.fprintf fmt "@[%a : ?@]" pp_var x
    | TVar (a, kind) ->
      Format.fprintf fmt "@[%a : %a@]" Tvar.print a Kind.print kind
  in
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_entry)
    ctx

let print fmt ctx = print_gen Var.print fmt ctx
let to_string ctx = Format.asprintf "%a" (print_gen Var.print_unique) ctx

module Test = struct
  let gen =
    let open QCheck.Gen in
    let* bindings = list_size (0 -- 5)
      (let* x = Var.Test.gen in
       oneof [
         (let* s = Sort.Test.gen in
          let* eff = Effect.Test.gen in
          pure (Term (x, s, eff)));
         pure (Unknown x);
       ])
    in
    pure (List.fold_left (fun acc b -> b :: acc) [] (List.rev bindings))

  let test =
    [ QCheck.Test.make ~name:"lookup finds extended variable"
        ~count:100
        QCheck.(triple (make Var.Test.gen) (make Sort.Test.gen) (make Effect.Test.gen))
        (fun (x, s, eff) ->
           let ctx = extend x s eff empty in
           match lookup x ctx with
           | Ok (s', eff') -> Sort.compare s s' = 0 && Effect.compare eff eff' = 0
           | Error _ -> false);

      QCheck.Test.make ~name:"lookup_tvar finds extended tvar"
        ~count:100
        QCheck.(pair (make Tvar.Test.gen) (make Kind.Test.gen))
        (fun (a, k) ->
           let ctx = extend_tvar a k empty in
           match lookup_tvar a ctx with
           | Some k' -> Kind.compare k k' = 0
           | None -> false);

      QCheck.Test.make ~name:"lookup of Unknown returns kind mentioning the var"
        ~count:100
        (QCheck.make Var.Test.gen)
        (fun x ->
           let ctx = extend_unknown x empty in
           match lookup x ctx with
           | Error k ->
             let s = Error.to_string (Error.locate_opt ~loc:None k) in
             let xname = Format.asprintf "%a" Var.print x in
             let n = String.length s and m = String.length xname in
             let rec aux i =
               i + m <= n
               && (String.sub s i m = xname || aux (i + 1)) in
             m = 0 || (n >= m && aux 0)
           | Ok _ -> false);

      QCheck.Test.make ~name:"lookup absent returns unbound variable error"
        ~count:100
        (QCheck.make Var.Test.gen)
        (fun x ->
           match lookup x empty with
           | Error k ->
             let s = Error.to_string (Error.locate_opt ~loc:None k) in
             let xname = Format.asprintf "%a" Var.print x in
             let n = String.length s and m = String.length xname in
             let rec aux i =
               i + m <= n
               && (String.sub s i m = xname || aux (i + 1)) in
             m = 0 || (n >= m && aux 0)
           | Ok _ -> false);

      QCheck.Test.make ~name:"extend_or_unknown Ok extends with sort"
        ~count:100
        QCheck.(triple (make Var.Test.gen) (make Sort.Test.gen) (make Effect.Test.gen))
        (fun (x, s, eff) ->
           let ctx = extend_or_unknown x (Ok s) eff empty in
           match lookup x ctx with
           | Ok (s', eff') -> Sort.compare s s' = 0 && Effect.compare eff eff' = 0
           | Error _ -> false);

      QCheck.Test.make ~name:"extend_or_unknown Error extends as Unknown"
        ~count:100
        QCheck.(pair (make Var.Test.gen) (make Effect.Test.gen))
        (fun (x, eff) ->
           let dummy_kind = Error.unbound_var x in
           let ctx = extend_or_unknown x (Error dummy_kind) eff empty in
           match lookup x ctx with
           | Error k ->
             let s = Error.to_string (Error.locate_opt ~loc:None k) in
             let n = String.length s
             and target = "sort is unknown"
             and target2 = "unknown variable sort" in
             let m = String.length target and m2 = String.length target2 in
             let rec has sub k =
               let l = String.length sub in
               k + l <= n
               && (String.sub s k l = sub || has sub (k + 1)) in
             (m > 0 && n >= m && has target 0)
             || (m2 > 0 && n >= m2 && has target2 0)
           | Ok _ -> false);
    ]
end
