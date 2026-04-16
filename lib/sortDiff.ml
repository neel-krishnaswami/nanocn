type shape_compare =
  | Diff of Sort.sort * Sort.sort
  | Same of (shape_compare, < loc : SourcePos.t >) Sort.sortF

(* Walk two sorts in lockstep. At each step we compare head
   constructors; when they agree we recurse, when they disagree we
   emit a [Diff]. For list-shaped children (Record args, App args),
   we only recurse if the lists have equal length — differing lengths
   mean the whole node disagrees. *)
let rec diff s1 s2 =
  match Sort.shape s1, Sort.shape s2 with
  | Sort.Int, Sort.Int -> Same Sort.Int
  | Sort.Bool, Sort.Bool -> Same Sort.Bool
  | Sort.Ptr t1, Sort.Ptr t2 -> Same (Sort.Ptr (diff t1 t2))
  | Sort.Pred t1, Sort.Pred t2 -> Same (Sort.Pred (diff t1 t2))
  | Sort.Record ts1, Sort.Record ts2
    when List.compare_lengths ts1 ts2 = 0 ->
    Same (Sort.Record (List.map2 diff ts1 ts2))
  | Sort.App (d1, ts1), Sort.App (d2, ts2)
    when Dsort.compare d1 d2 = 0
      && List.compare_lengths ts1 ts2 = 0 ->
    Same (Sort.App (d1, List.map2 diff ts1 ts2))
  | Sort.TVar a1, Sort.TVar a2
    when Tvar.compare a1 a2 = 0 ->
    Same (Sort.TVar a1)
  | _, _ -> Diff (s1, s2)

let dummy_info = (object method loc = SourcePos.dummy end :
                    < loc : SourcePos.t >)

let rec left = function
  | Diff (s1, _) -> s1
  | Same sf -> Sort.mk dummy_info (Sort.map_shape left sf)

let rec right = function
  | Diff (_, s2) -> s2
  | Same sf -> Sort.mk dummy_info (Sort.map_shape right sf)

(* Render one side of the diff with [Diff] subterms wrapped in a
   red-bold semantic tag. The side-selector [pick] decides which of
   the two sorts to display when we hit a [Diff] node. *)
let rec print_with_pick pick fmt c =
  match c with
  | Diff (s1, s2) ->
    Format.pp_open_stag fmt (Format.String_tag "red;bold");
    Sort.print fmt (pick s1 s2);
    Format.pp_close_stag fmt ()
  | Same sf ->
    print_shape pick fmt sf

and print_shape pick fmt = function
  | Sort.Int -> Format.fprintf fmt "Int"
  | Sort.Bool -> Format.fprintf fmt "Bool"
  | Sort.Ptr sc ->
    Format.fprintf fmt "@[<hov 2>Ptr@ %a@]" (print_with_pick pick) sc
  | Sort.Record [] -> Format.fprintf fmt "()"
  | Sort.Record [sc] ->
    Format.fprintf fmt "(%a *)" (print_with_pick pick) sc
  | Sort.Record scs ->
    Format.fprintf fmt "@[<hov 2>(%a)@]"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt " *@ ")
         (print_with_pick pick))
      scs
  | Sort.App (d, []) -> Dsort.print fmt d
  | Sort.App (d, scs) ->
    Format.fprintf fmt "@[<hov 2>%a(%a)@]" Dsort.print d
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         (print_with_pick pick))
      scs
  | Sort.Pred sc ->
    Format.fprintf fmt "@[<hov 2>Pred@ %a@]" (print_with_pick pick) sc
  | Sort.TVar a -> Tvar.print fmt a

let print_left fmt c = print_with_pick (fun s1 _ -> s1) fmt c
let print_right fmt c = print_with_pick (fun _ s2 -> s2) fmt c

module Test = struct
  (* Count how many [Diff] leaves live in a [shape_compare]. *)
  let rec num_diffs = function
    | Diff _ -> 1
    | Same sf ->
      let add acc sc = acc + num_diffs sc in
      (match sf with
       | Sort.Int | Sort.Bool | Sort.TVar _ -> 0
       | Sort.Ptr sc | Sort.Pred sc -> num_diffs sc
       | Sort.Record scs -> List.fold_left add 0 scs
       | Sort.App (_, scs) -> List.fold_left add 0 scs)

  let reflexive =
    QCheck.Test.make
      ~name:"SortDiff.diff s s has no Diff leaves"
      ~count:200
      (QCheck.make Sort.Test.gen)
      (fun s -> num_diffs (diff s s) = 0)

  let mismatch_means_compare_ne_zero =
    QCheck.Test.make
      ~name:"Every Diff carries sorts that compare non-equal"
      ~count:200
      (QCheck.make (QCheck.Gen.pair Sort.Test.gen Sort.Test.gen))
      (fun (s1, s2) ->
        let rec check = function
          | Diff (a, b) -> Sort.compare a b <> 0
          | Same sf ->
            let go acc sc = acc && check sc in
            (match sf with
             | Sort.Int | Sort.Bool | Sort.TVar _ -> true
             | Sort.Ptr sc | Sort.Pred sc -> check sc
             | Sort.Record scs -> List.fold_left go true scs
             | Sort.App (_, scs) -> List.fold_left go true scs)
        in
        check (diff s1 s2))

  let left_recovers_s1 =
    QCheck.Test.make
      ~name:"left (diff s1 s2) compares equal to s1"
      ~count:200
      (QCheck.make (QCheck.Gen.pair Sort.Test.gen Sort.Test.gen))
      (fun (s1, s2) -> Sort.compare (left (diff s1 s2)) s1 = 0)

  let right_recovers_s2 =
    QCheck.Test.make
      ~name:"right (diff s1 s2) compares equal to s2"
      ~count:200
      (QCheck.make (QCheck.Gen.pair Sort.Test.gen Sort.Test.gen))
      (fun (s1, s2) -> Sort.compare (right (diff s1 s2)) s2 = 0)

  let test = [
    reflexive;
    mismatch_means_compare_ne_zero;
    left_recovers_s1;
    right_recovers_s2;
  ]
end
