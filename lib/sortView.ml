type 'a t = ('a, Error.kind) result

(** Project an arbitrary ['info Sort.t] (where ['info] carries at least
    [loc : SourcePos.t]) down to a [Sort.sort] so it can be embedded in
    [K_construct_sort_mismatch]'s [got] field for error reporting. *)
let project s =
  Sort.map (fun i -> (object method loc = i#loc end)) s

let mismatch ~construct ~expected_shape s =
  Error (Error.K_construct_sort_mismatch
           { construct; expected_shape; got = project s })

let propagate r = (Error r : 'a t)

module Get = struct
  let int ~construct = function
    | Error _ as e -> e
    | Ok s ->
      (match Sort.shape s with
       | Sort.Int -> Ok ()
       | _ -> mismatch ~construct ~expected_shape:"Int" s)

  let bool ~construct = function
    | Error _ as e -> e
    | Ok s ->
      (match Sort.shape s with
       | Sort.Bool -> Ok ()
       | _ -> mismatch ~construct ~expected_shape:"Bool" s)

  let ptr ~construct = function
    | Error _ as e -> e
    | Ok s ->
      (match Sort.shape s with
       | Sort.Ptr inner -> Ok inner
       | _ -> mismatch ~construct ~expected_shape:"Ptr _" s)

  let pred ~construct = function
    | Error _ as e -> e
    | Ok s ->
      (match Sort.shape s with
       | Sort.Pred inner -> Ok inner
       | _ -> mismatch ~construct ~expected_shape:"Pred _" s)

  let record ~construct n = function
    | Error e -> List.init n (fun _ -> propagate e)
    | Ok s ->
      (match Sort.shape s with
       | Sort.Record ts when List.length ts = n ->
         List.map (fun t -> Ok t) ts
       | _ ->
         let err = mismatch ~construct ~expected_shape:"Record _" s in
         List.init n (fun _ -> err))

  let app ~construct = function
    | Error e -> propagate e, []
    | Ok s ->
      (match Sort.shape s with
       | Sort.App (d, ts) -> Ok d, List.map (fun t -> Ok t) ts
       | _ ->
         let err = mismatch ~construct ~expected_shape:"datasort/datatype application" s in
         err, [])

  let tvar ~construct = function
    | Error _ as e -> e
    | Ok s ->
      (match Sort.shape s with
       | Sort.TVar a -> Ok a
       | _ -> mismatch ~construct ~expected_shape:"type variable" s)
end

module Build = struct
  let int info = function
    | Error _ as e -> e
    | Ok () -> Ok (Sort.mk info Sort.Int)

  let bool info = function
    | Error _ as e -> e
    | Ok () -> Ok (Sort.mk info Sort.Bool)

  let ptr info = function
    | Error _ as e -> e
    | Ok inner -> Ok (Sort.mk info (Sort.Ptr inner))

  let pred info = function
    | Error _ as e -> e
    | Ok inner -> Ok (Sort.mk info (Sort.Pred inner))

  let record info xs =
    match Util.result_list xs with
    | Error _ as e -> e
    | Ok ts -> Ok (Sort.mk info (Sort.Record ts))

  let app info d xs =
    match d, Util.result_list xs with
    | Error _ as e, _ -> e
    | _, (Error _ as e) -> e
    | Ok d, Ok ts -> Ok (Sort.mk info (Sort.App (d, ts)))

  let tvar info = function
    | Error _ as e -> e
    | Ok a -> Ok (Sort.mk info (Sort.TVar a))
end

module Test = struct
  let dummy_info = object method loc = SourcePos.dummy end
  let mk_sort s = Sort.mk dummy_info s

  let int_sort = mk_sort Sort.Int
  let bool_sort = mk_sort Sort.Bool
  let ptr_int = mk_sort (Sort.Ptr int_sort)
  let pair_int = mk_sort (Sort.Record [int_sort; int_sort])
  let pred_int = mk_sort (Sort.Pred int_sort)

  let test = [
    QCheck.Test.make
      ~name:"SortView.Get.int: Ok Int -> Ok ()"
      ~count:1 QCheck.unit
      (fun () ->
         match Get.int ~construct:"x" (Ok int_sort) with
         | Ok () -> true
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Get.int: Ok Bool -> Error"
      ~count:1 QCheck.unit
      (fun () ->
         match Get.int ~construct:"x" (Ok bool_sort) with
         | Error _ -> true
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Get.int: Error in -> Error out"
      ~count:1 QCheck.unit
      (fun () ->
         let dummy = Error.K_unbound_var (fst (Var.mk "z" SourcePos.dummy Var.empty_supply)) in
         match Get.int ~construct:"x" (Error dummy) with
         | Error e when e = dummy -> true
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Get.ptr: round-trip on Ptr _"
      ~count:1 QCheck.unit
      (fun () ->
         match Get.ptr ~construct:"x" (Ok ptr_int) with
         | Ok inner -> Sort.compare inner int_sort = 0
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Get.record: arity match"
      ~count:1 QCheck.unit
      (fun () ->
         let xs = Get.record ~construct:"tuple" 2 (Ok pair_int) in
         List.length xs = 2 && List.for_all Result.is_ok xs);

    QCheck.Test.make
      ~name:"SortView.Get.record: arity mismatch yields list of n errors"
      ~count:1 QCheck.unit
      (fun () ->
         let xs = Get.record ~construct:"tuple" 3 (Ok pair_int) in
         List.length xs = 3 && List.for_all Result.is_error xs);

    QCheck.Test.make
      ~name:"SortView.Get.record: wrong shape yields list of n errors"
      ~count:1 QCheck.unit
      (fun () ->
         let xs = Get.record ~construct:"tuple" 2 (Ok int_sort) in
         List.length xs = 2 && List.for_all Result.is_error xs);

    QCheck.Test.make
      ~name:"SortView.Build.int: round-trip"
      ~count:1 QCheck.unit
      (fun () ->
         match Build.int dummy_info (Ok ()) with
         | Ok s -> Sort.compare s int_sort = 0
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Build.record: round-trip on Ok inputs"
      ~count:1 QCheck.unit
      (fun () ->
         match Build.record dummy_info [Ok int_sort; Ok int_sort] with
         | Ok s -> Sort.compare s pair_int = 0
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Build.record: any Error input -> Error output"
      ~count:1 QCheck.unit
      (fun () ->
         let dummy = Error.K_unbound_var (fst (Var.mk "z" SourcePos.dummy Var.empty_supply)) in
         match Build.record dummy_info [Ok int_sort; Error dummy] with
         | Error _ -> true
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView round-trip: Get.pred then Build.pred = Ok original"
      ~count:1 QCheck.unit
      (fun () ->
         let inner = Get.pred ~construct:"x" (Ok pred_int) in
         match Build.pred dummy_info inner with
         | Ok s -> Sort.compare s pred_int = 0
         | _ -> false);
  ]
end
