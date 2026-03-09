(** Program structure: function declarations followed by a main expression. *)

type 'a decl = {
  name : Var.t;
  param : Var.t;
  arg_ty : Typ.ty;
  ret_ty : Typ.ty;
  eff : Effect.t;
  body : 'a;
  loc : SourcePos.t;
}

type 'a t = {
  decls : 'a decl list;
  main : 'a;
  loc : SourcePos.t;
}

val map : ('a -> 'b) -> 'a t -> 'b t
val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

module Test : sig
  val test : QCheck.Test.t list
end
