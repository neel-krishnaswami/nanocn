# LSP Improvements

## Type-Directed Elaboration Actions

I want to add basic support for type-directed editor actions to the LSP mode. 

This consists of two parts, pattern expansion and hole expansion. 


### Pattern expansion 

When the cursor is over a variable pattern, it will have access to the 
pattern's type, and can use it to *expand* the pattern. 

1. For core patterns: if the pattern variable "x" is of a tuple type (τ1, ..., τn), 
   the pattern "x" should be replaced with "(x1, ..., xn)", and all occurences of 
   "x" in the term should be substituted with (x1, ..., xn) in the scope of x. 

2. For logical patterns: no editor actions are currently needed. 

3. For resource patterns. The expansion of a pattern variable "x" depends on 
   the shape of the resource (ce @ ce'). If ce is: 

	* return ce:          

		"x" ~~> "return [xeq]"

	    Substitute [return xeq/x] in the scope of x. 

    * take y = ce1; ce2: 

	    "x" ~~> "take(y, x1); x2"
		
	    Substitute [take(x1,x2)/x] in the scope of x. 

    * let y = ce1; ce2: 

	    "x" ~~> "let[eqi] y; x1"

         Substitute [(let [eqi'] y; x1)/x] in the scope of x. 

    * let (y1, ..., yn) = ce1; ce2 
	
		"x" ~~> "let[eqi] (y1, ..., yn); x1"

	    Substitute [let[eqi'] (y1, ..., yn); x1/x] in the scope of x. 

    * f(ce) 

        "x" ~~> "unfold; x1"

	    Substitute [unfold; x1/x] in the scope of x 

    
### Hole expansion 

Next, I want to use the type of a hole to expand it into several smaller holes,
using the expected type coming from the context to drive the expansion. 


