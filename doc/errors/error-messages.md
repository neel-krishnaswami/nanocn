# Improving Error Messages 

## Parse Errors 

1. Download https://gallium.inria.fr/~fpottier/menhir/manual.html#sec%3Amessages%3Aformat. 
2. Read this, focusing particularly on Section 11 ("Error h andling: the new way"), and write
   a summary of how to write error good parser messages. 
3. Use this to generate a .messages file, and write error messages for each erroneous state. 
4. Update the build system to generate messages. 

   

## Type Errors 

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

   - If it is a type mismatch, print both the inferred and expected type, using 
	 antiunification to figure out the subterms where they differ, and highlighting those. 

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

	
