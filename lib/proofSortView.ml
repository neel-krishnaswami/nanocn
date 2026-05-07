type 'a t = 'a option

module Get = struct
  let nil = function
    | Some [] -> Some ()
    | _ -> None

  let comp = function
    | Some (ProofSort.Comp { var; sort; eff; _ } :: rest) ->
      (Some var, Some sort, Some eff, Some rest)
    | _ -> (None, None, None, None)

  let log = function
    | Some (ProofSort.Log { prop; _ } :: rest) ->
      (Some prop, Some rest)
    | _ -> (None, None)

  let res = function
    | Some (ProofSort.Res { pred; value; _ } :: rest) ->
      (Some pred, Some value, Some rest)
    | _ -> (None, None, None)

  let depres = function
    | Some (ProofSort.DepRes { bound_var; pred; _ } :: rest) ->
      (Some bound_var, Some pred, Some rest)
    | _ -> (None, None, None)
end

module Build = struct
  let nil _info = function
    | Some () -> Some []
    | None -> None

  let comp info var_o sort_o eff_o tail_o =
    match var_o, sort_o, eff_o, tail_o with
    | Some var, Some sort, Some eff, Some tail ->
      Some (ProofSort.Comp { info; var; sort; eff } :: tail)
    | _ -> None

  let log info prop_o tail_o =
    match prop_o, tail_o with
    | Some prop, Some tail ->
      Some (ProofSort.Log { info; prop } :: tail)
    | _ -> None

  let res info pred_o value_o tail_o =
    match pred_o, value_o, tail_o with
    | Some pred, Some value, Some tail ->
      Some (ProofSort.Res { info; pred; value } :: tail)
    | _ -> None

  let depres info bound_var_o pred_o tail_o =
    match bound_var_o, pred_o, tail_o with
    | Some bound_var, Some pred, Some tail ->
      Some (ProofSort.DepRes { info; bound_var; pred } :: tail)
    | _ -> None
end

module Test = struct
  let test = []
end
