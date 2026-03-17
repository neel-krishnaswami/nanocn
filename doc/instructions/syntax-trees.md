# Instructions for writing syntax trees

1. Split recursive types for syntax trees into a doubly-parameterized shape type, and something
   which ties the recursive knot. There should be two type parameters, one for what will become
   recursive data, and one for auxilliary information (like location or typing) shared by all
   subterms. 

   Instead of writing: 

     ```
     type tp = Tuple of tp list | Arrow of tp * tp | Int 
     ``` 

     first write 

     ```
     type ('a, 'b) tpF = Tuple of 'a list | Arrow 'a * 'a | Int 
     ```

     In the API, expose the functions
     ```
     map_tpF : ('a -> 'b) -> ('a,'c) tpF -> ('b, 'c) tpF
     map_tpF_info : ('b -> 'c) -> ('a, 'b) tpF -> ('a, 'c) tpF 
     ```` 
     in the interface. 

     Then, to tie the knot, define: 

     ```type 'b tp = In of 'b * ('b t ,'b) tpF```

     and expose a function: 

     ```map : ('a -> 'b) -> 'a t -> 'b t 

2. The constructor `In` should not be exposed in the API. Instead, the .mli file should
   make that type abstract, and expose accessors: 

   ```
   extract : 'a tp -> 'a 
   shape : 'a tp -> 'a tp tpF 
   ```

3. The double parameterization is very useful when one term type is used inside another. 

     ```type ('a, 'b) expF = Var of Var.t | Lam of Var.t * 'b tp * 'a | App of 'a * 'a 

        type 'b exp = In of 'b * ('b exp, 'b) expF

     ```

   This ensures that a single type declaration
     ```
        type expr = <loc : SourcePos.t> exp
     ```
   guarantees that even subterms which are types will have location information.

   By using an object for the auxilliary information, multiple forms of information
   (eg, source position, the context, the type, etc.)can be accessed with a single flat
   method call `(extract v)#loc`. 

4. ALWAYS make sure that syntax trees carry location information. 

   No transformation on syntax trees should ever lose location information. If you need to invent
   a dummy location, first ask the user. 

