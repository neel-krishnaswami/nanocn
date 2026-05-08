type var = string
type msg = string 

type tp = 
  | Arrow of tp * tp 
  | Bool 
  | Prod of tp * tp 

type 'a expF = 
  | Var of var
  | Let of var * 'a * 'a
  | Annot of 'a * tp 
  | BLit of bool
  | If of 'a * 'a * 'a 
  | Lam of var * 'a 
  | App of 'a * 'a
  | Pair of 'a * 'a 
  | Fst of 'a 
  | Snd of 'a 

type 'a exp = In of 'a * 'a exp expF 

(* In(1, App(In(2, Var "x"), In(3, Var "y"))) *)

let info (In(a, _)) = a 
let shape (In(_, e)) = e 

let mk a e = In(a, e)

module View = struct 
  type 'a t = { 
      extract : tp option -> 'a;
      build : 'a -> tp option
    }

  let bool = 
    let extract = function
      | Some Bool -> Some () 
      | _ -> None in
    let build = function
      | Some () -> Some Bool
      | None -> None
    in
    {extract; build}

  let arrow = 
    let extract = function
      | Some (Arrow(tp1, tp2)) -> (Some tp1, Some tp2)
      | _ -> (None, None) in 
    let build = function 
      | (Some tp1, Some tp2) -> Some(Arrow(tp1, tp2))
      | _ -> None
    in 
    {extract; build}

  let prod = 
    let extract = function
      | Some (Prod(tp1, tp2)) -> (Some tp1, Some tp2)
      | _ -> (None, None) in 
    let build = function 
      | (Some tp1, Some tp2) -> Some(Prod(tp1, tp2))
      | _ -> None 
    in
    {extract; build}
end

type ctx = (var * tp option) list 

let lookup ctx x = 
  match List.assoc_opt x ctx with
  | None -> Error "Unbound variable" 
  | Some None -> Error "Unknown type of variable"
  | Some (Some tp) -> Ok tp 

let typeof e = Result.to_option (info e) 


let rec synth : ctx -> unit exp -> (tp, string) result exp = 
  fun ctx e -> 
  (match shape e with
   | Var x -> mk (lookup ctx x) (Var x)
   | Annot (e, tp) -> 
      let e' = check ctx e (Some tp) in 
      mk (Ok tp) (Annot(e', tp))
   | App (e1, e2) -> 
      let e1' = synth ctx e1 in 
      let (tp1, tp2) = View.arrow.extract (typeof e1') in 
      let e2' = check ctx e2 tp1 in 
      let tp2 = Option.to_result ~none:"Expected function argument type" tp2 in 
      mk tp2 (App(e1', e2'))
   | _ -> check ctx e None)
and check : ctx -> unit exp -> tp option -> (tp, string) result exp = 
  fun ctx e tp -> 
    match shape e with 
    | Lam(x, e) -> 
       let (tp1, tp2) = View.arrow.extract tp in 
       let e' = check ((x, tp1) :: ctx) e tp2 in 
       let tp' = Option.to_result ~none:"Expected function type" (View.arrow.build(tp1, tp2)) in
       mk tp' (Lam(x, e'))
    | BLit b -> 
       let tp' = View.bool.extract tp in
       let tp'' = Option.to_result ~none:"Expected boolean type" (View.bool.build tp') in
       mk tp'' (BLit b)
    | If(e1, e2, e3) -> 
       let e1' = check ctx e1 (Some Bool) in 
       let e2' = check ctx e2 tp in 
       let e3' = check ctx e3 tp in 
       let tp' = Option.to_result ~none:"If-then-else is checking" tp in 
       mk tp' (If(e1', e2', e3'))
    | Let(x, e1, e2) -> 
       let e1' = synth ctx e1 in 
       let tp1 = typeof e1' in 
       let e2' = check ((x, tp1) :: ctx) e2 tp in
       let tp' = Option.to_result ~none:"Let-in is checking" tp in 
       mk tp' (Let(x, e1', e2'))
    | _ -> let e' = synth ctx e in 
           if tp = typeof e' then 
             e' 
           else 
             mk (Error "type mismatch") (shape e')

       
