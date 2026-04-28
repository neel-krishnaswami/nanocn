let mismatch ~construct ~expected_shape s =
  Error (Error.K_construct_sort_mismatch
           { construct; expected_shape; got = s })

let get_pred ~construct s =
  match Sort.shape s with
  | Sort.Pred t -> Ok t
  | _ -> mismatch ~construct ~expected_shape:"Pred _" s

let get_app ~construct s =
  match Sort.shape s with
  | Sort.App (d, ts) -> Ok (d, ts)
  | _ -> mismatch ~construct ~expected_shape:"datasort/datatype application" s

let get_record ~construct s =
  match Sort.shape s with
  | Sort.Record ts -> Ok ts
  | _ -> mismatch ~construct ~expected_shape:"Record _" s
