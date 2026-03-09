Instructions to Claude for writing OCaml code: 

0. When writing code, do these things first: 

   1. Write a plan with high-level architectural decisions. Analyze this plan for defects, 
      and keep fixing them until no obvious deficiencies remain. 

   2. Write a detailed design document, with design choices for each module. Again, before
      proceeding to implementaiton, analyze the design for obvious flaws before proceeding. 

   3. If the detailed design reveals a key flaw, consider whether the high-level plan needs
      to be revised. 

   4. Documentation in the doc/ directory MUST remain in sync with the implementation. Whenever
      a change to the code is made, the Ott file for the syntax and typing rules must be updated 
      as well. 

1. Programs should be composed of small modules, each implementing a single concern or 
   data structure.

2. Write .mli files first, before writing any part of a module.

   - .mli files should emphasize the algebraic structure of the data structure. 

   - Name the primary type of an .mli file as `t`, so that clients can refer to it as 
     `Foo.t`, or `'a Foo.t`. 

   - Unless otherwise necessary, hide the implementation type. 

   - Parameterized types of the form `'a t` should expose a `map : ('a -> 'b) -> 'a t -> 'b t`
     primitive in their interface. 

   - If a type constructor has monadic structure, then define `return : 'a -> 'a t` and
     `(let+) : 'a -> ('a -> 'b t) -> 'b t` operations.

   - If a type can be ordered, then expose a `compare : t -> t > int` primitive. 

   - If a type can be printed, expose a `print : Format.formatter -> t -> unit` method in the 
     interface. Use the Format module's indentation directives to ensure that print methods are
     nicely laid out.

   - If a module exposes a parameterized type, give parameterized comparison and printing
     functions. 

   - Every module should have property-based tests using `QCheck`, based on the
     invariants of the module. In a Test submodule, expose generators, properties,
     and a test field using the QCheck library. 

3. Here are some bad language features to avoid: 

   - NEVER use Obj.magic, or any other feature which can break type safety. 

   - NEVER use generic equality, since this violates data abstraction. Always use a 
     type-specific `compare` operation.

3. Unless explicitly instructed otherwise, DO NOT write code which uses effects.

   - Use result types instead of exceptions. 

   - Permission to use mutable data structures is granted on a per-module basis, and 
     permission in one module does not grant it in any other. 

   - Do not perform IO operations, except in the top-level main function. 

3. Write programs by pattern matching over data structures. Avoid using partial accessors. 
  
4. Higher-order functions should be used sparingly, in idiomatic ways.

   - Introducing monadic code to eliminate repeated nested pattern matches is acceptable. 
   - Use of map, filter, and other algebraically well-behaved functions is acceptable. 
   - Avoid the use of folds, because they offer no reasoning advantages over explicit
     structural recursion.


5. When writing types for syntax trees: 

   - Split recursive types for syntax trees into a parameterized shape type, and something which
     ties the recursive knot.

     Instead of writing: 

     ```type exp = Lit of num | Add of exp * exp```

     first write 

     ```type 'a expF = Lit of num | Add of 'a * 'a
     ```

     This lets you expose a `map_expF : ('a -> 'b) -> 'a expF -> 'b expF` function in the interface. 

   - ALWAYS make sure that syntax trees carry location information. No transformation on syntax trees
     should ever lose location information. 

     So to tie the knot for 

     ```type 'a expF = Lit of num | Add of 'a * 'a
     ```

     instead of writing 


     ```type exp = In of exp expF```

     Write 

     ```type 'b exp = In of 'b exp expF * 'b 

        type expr = <loc : SourcePos.t> exp

     ```

    By using an object, multiple forms of information (eg, source position, the context, the type, etc.)
    can be accessed with a single flat method call `(extract v)#loc`. 


