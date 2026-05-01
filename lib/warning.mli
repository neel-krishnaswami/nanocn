(** Compile-time warnings — non-fatal diagnostics that the user
    should see but that don't fail compilation.

    Distinct from [Error.t]: warnings are reported with a "Warning"
    header and severity=Hint at the LSP layer; they don't stop the
    build.  Each warning carries a source position and a structured
    [kind] so renderers can produce specific guidance.

    Warnings live on info objects (pat info, typed_info), so they
    travel with the AST/tree as it flows through resolve, typecheck,
    and elaborate.  The driver collects every warning attached to
    nodes anywhere in the typed tree at the end of compilation. *)

type t

type kind =
  | K_pat_var_shadowed of { name : string }
    (** A pattern variable's binding is unreachable because the same
        name is bound again later in the same pattern.  The warning
        is attached to the earlier (shadowed) [PVar] binder, not the
        later (shadowing) one. *)

val structured : loc:SourcePos.t -> kind -> t
(** [structured ~loc k] builds a warning at [loc] with kind [k]. *)

val pat_var_shadowed : loc:SourcePos.t -> name:string -> t

val loc : t -> SourcePos.t
val print : SourceExcerpt.registry -> Format.formatter -> t -> unit
val to_string : t -> string

module Test : sig
  val test : QCheck.Test.t list
end
