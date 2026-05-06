type 'a t = 'a option

let project s =
  Sort.map (fun i -> (object method loc = i#loc end)) s

module Get = struct
  let int = function
    | None -> None
    | Some s ->
      (match Sort.shape s with
       | Sort.Int -> Some ()
       | _ -> None)

  let bool = function
    | None -> None
    | Some s ->
      (match Sort.shape s with
       | Sort.Bool -> Some ()
       | _ -> None)

  let ptr = function
    | None -> None
    | Some s ->
      (match Sort.shape s with
       | Sort.Ptr inner -> Some inner
       | _ -> None)

  let pred = function
    | None -> None
    | Some s ->
      (match Sort.shape s with
       | Sort.Pred inner -> Some inner
       | _ -> None)

  let record n = function
    | None -> List.init n (fun _ -> None)
    | Some s ->
      (match Sort.shape s with
       | Sort.Record ts when List.length ts = n ->
         List.map (fun t -> Some t) ts
       | _ ->
         List.init n (fun _ -> None))

  let app = function
    | None -> None, []
    | Some s ->
      (match Sort.shape s with
       | Sort.App (d, ts) -> Some d, List.map (fun t -> Some t) ts
       | _ -> None, [])

  let tvar = function
    | None -> None
    | Some s ->
      (match Sort.shape s with
       | Sort.TVar a -> Some a
       | _ -> None)
end

module Build = struct
  let int info = function
    | None -> None
    | Some () -> Some (Sort.mk info Sort.Int)

  let bool info = function
    | None -> None
    | Some () -> Some (Sort.mk info Sort.Bool)

  let ptr info = function
    | None -> None
    | Some inner -> Some (Sort.mk info (Sort.Ptr inner))

  let pred info = function
    | None -> None
    | Some inner -> Some (Sort.mk info (Sort.Pred inner))

  let rec sequence_options = function
    | [] -> Some []
    | None :: _ -> None
    | Some x :: rest ->
      (match sequence_options rest with
       | None -> None
       | Some xs -> Some (x :: xs))

  let record info xs =
    match sequence_options xs with
    | None -> None
    | Some ts -> Some (Sort.mk info (Sort.Record ts))

  let app info d xs =
    match d, sequence_options xs with
    | None, _ | _, None -> None
    | Some d, Some ts -> Some (Sort.mk info (Sort.App (d, ts)))

  let tvar info = function
    | None -> None
    | Some a -> Some (Sort.mk info (Sort.TVar a))
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
      ~name:"SortView.Get.int: Some Int -> Some ()"
      ~count:1 QCheck.unit
      (fun () ->
         match Get.int (Some int_sort) with
         | Some () -> true
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Get.int: Some Bool -> None"
      ~count:1 QCheck.unit
      (fun () ->
         match Get.int (Some bool_sort) with
         | None -> true
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Get.int: None in -> None out"
      ~count:1 QCheck.unit
      (fun () ->
         match Get.int None with
         | None -> true
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Get.ptr: round-trip on Ptr _"
      ~count:1 QCheck.unit
      (fun () ->
         match Get.ptr (Some ptr_int) with
         | Some inner -> Sort.compare inner int_sort = 0
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Get.record: arity match"
      ~count:1 QCheck.unit
      (fun () ->
         let xs = Get.record 2 (Some pair_int) in
         List.length xs = 2 && List.for_all Option.is_some xs);

    QCheck.Test.make
      ~name:"SortView.Get.record: arity mismatch yields list of n Nones"
      ~count:1 QCheck.unit
      (fun () ->
         let xs = Get.record 3 (Some pair_int) in
         List.length xs = 3 && List.for_all Option.is_none xs);

    QCheck.Test.make
      ~name:"SortView.Get.record: wrong shape yields list of n Nones"
      ~count:1 QCheck.unit
      (fun () ->
         let xs = Get.record 2 (Some int_sort) in
         List.length xs = 2 && List.for_all Option.is_none xs);

    QCheck.Test.make
      ~name:"SortView.Build.int: round-trip"
      ~count:1 QCheck.unit
      (fun () ->
         match Build.int dummy_info (Some ()) with
         | Some s -> Sort.compare s int_sort = 0
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Build.record: round-trip on Some inputs"
      ~count:1 QCheck.unit
      (fun () ->
         match Build.record dummy_info [Some int_sort; Some int_sort] with
         | Some s -> Sort.compare s pair_int = 0
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView.Build.record: any None input -> None output"
      ~count:1 QCheck.unit
      (fun () ->
         match Build.record dummy_info [Some int_sort; None] with
         | None -> true
         | _ -> false);

    QCheck.Test.make
      ~name:"SortView round-trip: Get.pred then Build.pred = Some original"
      ~count:1 QCheck.unit
      (fun () ->
         let inner = Get.pred (Some pred_int) in
         match Build.pred dummy_info inner with
         | Some s -> Sort.compare s pred_int = 0
         | _ -> false);
  ]
end
