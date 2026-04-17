; tags.scm — Imenu / document-symbol entries for nanoCN

(fun_decl
  name: (lower_ident) @name) @definition.function

(rfun_decl
  name: (lower_ident) @name) @definition.function

(sort_decl
  name: (upper_label) @name) @definition.type

(type_decl
  name: (upper_label) @name) @definition.type

(main_decl) @definition.function
