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

5. For mutually-recursive syntax tree types, add additional parameterization. Instead of
   writing:

   type intexp = 
      | ILit of int 
      | Plus of intexp * intexp
      | Negate of intexp 
      | Times of intexp 
      | If of boolexp * intexp * intexp 
  and boolexp = 
      | BLit of bool
      | And of boolexp * boolexp
      | Not of boolexp * boolexp 
      | Leq of intexp * intexp 

   Write: 

      type ('int, 'bool) intexpF = 
         | ILit of int 
         | Plus of 'int * 'int
         | Negate of 'int 
         | Times of 'int 
         | If of 'bool * 'int * 'int 
   
      type ('int, 'bool) boolexpF = 
         | BLit of bool
         | And of 'bool * 'bool
         | Not of 'bool
         | Leq of 'int * 'int 

     Construct mapping operations for this as follows: 
   
       type ('int1, 'int2, 'bool1, 'bool2) mapper = {
         intexp : 'int1 -> 'int2; 
         boolexp : 'bool1 -> 'bool2; 
       }
   
     let default : ('a, 'a, 'b, 'b, 'c, 'c) mapper = 
       let id x = x in
       {intexp = id; boolexp = id; info = id}

     let map_intexpf map = function 
       | ILit n -> ILit n 
       | Plus (i1, i2) -> Plus(map.intexp i1, map.intexp i2)
       | Negate i -> Negate (map.intexp i)
       | Times (i1, i2) -> Times(map.intexp i1, map.intexp i2)
       | If(b, i1, i2) -> If(map.boolexp b, map.intexp i1, map.intexp i2)

     let map_boolexpF map = function
         | BLit b -> BLit b 
         | And(b1, b2) -> And(map.boolexp b1, map.boolexp b2)
         | Not b -> Not (map.boolexp b)
         | Leq(i1, i2) -> Leq(map.

  Tie the knot with a mutually recursive fixed point: 

    type 'info intexp = In of 'info * ('info intexp, 'info boolexp) intexpF
    and 'info boolexp = In of 'info * ('info intexp, 'info boolexp) boolexpF

  Now, if we wanted to negate every boolean literal in a program, we could do so as follows: 

  let rec negate_boolexp (In(info, shape)) =
     let map = {intexp = negate_intexp; boolexp = negate_boolexp} in
     match shape with 
     | BLit b -> In(info, BLit (not b))
     | shape  -> In(info, map_boolexp shape)
  and negate_intexp (In(info, shape)) = 
    let map = {intexp = negate_intexp; boolexp = negate_boolexp} in
    In(info, map_intexp map shape)


     
 


