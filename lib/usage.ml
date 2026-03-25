type t = Used | Avail | Opt

let meet u1 u2 =
  match u1, u2 with
  | Used, Used -> Some Used
  | Avail, Avail -> Some Avail
  | Opt, Opt -> Some Opt
  | Used, Opt | Opt, Used -> Some Used
  | Avail, Opt | Opt, Avail -> Some Opt
  | Used, Avail | Avail, Used -> None

let is_avail = function
  | Avail | Opt -> true
  | Used -> false

let is_zero = function
  | Used | Opt -> true
  | Avail -> false

let affinize = function
  | Used -> Used
  | Avail -> Opt
  | Opt -> Opt

let compare a b =
  let to_int = function Used -> 0 | Avail -> 1 | Opt -> 2 in
  Int.compare (to_int a) (to_int b)

let print fmt = function
  | Used -> Format.fprintf fmt "used"
  | Avail -> Format.fprintf fmt "avail"
  | Opt -> Format.fprintf fmt "opt"

module Test = struct
  let gen =
    QCheck.Gen.oneofl [Used; Avail; Opt]

  let arb = QCheck.make gen

  let test =
    [ QCheck.Test.make ~name:"usage meet is commutative"
        ~count:20
        QCheck.(pair arb arb)
        (fun (u1, u2) ->
           match meet u1 u2, meet u2 u1 with
           | None, None -> true
           | Some a, Some b -> compare a b = 0
           | _ -> false);

      QCheck.Test.make ~name:"usage is_avail and is_zero partition"
        ~count:10
        arb
        (fun u ->
           match u with
           | Avail -> is_avail u && not (is_zero u)
           | Used -> not (is_avail u) && is_zero u
           | Opt -> is_avail u && is_zero u);
    ]
end
