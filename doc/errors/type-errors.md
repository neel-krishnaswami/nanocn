# Better Type Errors

## Compiler Error Messages

1. We do not want errors to be unstructured strings. 

2. Instead, construct an error datatype. This type should have one branch for each 
   failing case in the typechecker. 

	Each case should have 
	1. The rule in which each error occurred, the premise which failed, and the 
		kind of failure it is. (These should all be datatypes!) 
	2. All the information available to the typechecking function at that point, such 
       as the current source position, the context, the expected type, and so on. 

3. There should be a single function which prints error values. It should print: 

   - A high level description of the error

   - The lines where the erroneous subterm occurs, plus a line or two of context before and
	 after, highlighting the erroneous term. 

   - If it is a type mismatch, print both the inferred and expected type. Compare the 
	 subterms structurally, and identify 

   - If it is a variable out-of-scope error, print the name of the variable. 

   - If it is a misused variable (wrong type, usage, effect, etc), print the location
	 where the variable was bound, as well. 

   - If it is a pattern match coverage failure, give an example shape which would
	 fail to match. (Eg, the shape `(Cons(_, Cons(_, )))` might not fail to be covered
	 by a match on a list, if the two patterns are `Nil()` and `Cons(x, Nil())`.) 

	 This will require tracking the constructor at each step of destructing a matrix of 
	 patterns. 

   - If a pattern has a constructor from an unexpected type, give the type that the 
	 pattern is from, and the type of the scrutinee. 


# How to identify differing subterms 

Suppose we have a shape functor `('a, 'b) shape`, with a fixed point `'b t`. 

Then, we can define a comparison type: 

   type 'b shape_compare = 
     | Diff of 'b t * 'b t 
     | Same of ('b shape_compare, 'b) shape 

and define a function `diff` which identifies the differing subterms. For 
example, for 'b Sort.t, we could write a comparison function `diff` which looks like: 

```
let rec diff t1 t2 = 
  match (shape t1, shape t2) with
  | Int, Int -> Same Int
  | Bool, Bool -> Same Bool
  | Ptr t1', Ptr t2' -> Same (Ptr (diff t1' t2'))
  | Record ts1, Record ts2 -> Same (Record (List.map2 diff ts1 ts2))
  | App(d1, ts1), App(d2, ts2) when ((DSort.compare d1 d2 = 0) -> 
      Same (App(d1, List.map2 diff ts1 ts2))
  | Pred t1', Pred t2' -> Same (Pred (diff t1' t2'))
  | TVar a1, TVar a2 when (TVar.compare a1 a2 = 0) -> Same (TVar a1)
  | _, _ -> Diff(t1, t2)

```

This makes it possible to know where to highlight differences during printing.

