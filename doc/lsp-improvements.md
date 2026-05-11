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

#### Core expressions: 

Holes occur in checking positions, and are expanded by the type they are 
checked. If a hole $hole is checked at: 

* (τ1, ..., τn): 

  rewrite $hole to ($hole1, ... $holen)

* for all other types, no rewrite

#### Logical expressions: 

$hole expands to "auto"

#### Resource expressions: 

Holes in checking positions are expanded according to the type they are 
checked at: 

* return ce:          

  $hole expands to "return auto"

* take y = ce1; ce2: 

  $hole expands to "take($hole1, $hole2)"
	
* let y = ce1; ce2: 

   $hole expands to "let[eqi] y; $hole"

* let (y1, ..., yn) = ce1; ce2 

  $hole expands to "let[eqi] (y1, ..., yn); $hole"

* f(ce) 

  $hole expands to "unfold; $hole"

### Refined expressions: 

A hole at a refined type expands to a sequence of core/logical/resource holes, 
wrapped in parentheses: 

The sequence is generated as follows, based on the refined typed Pf and the current index i:

* If Pf is nil, then we generate the empty sequence: 

* If Pf is (x : τ [eff], Pf'), then we generate $holei, followed by the sequence for Pf' with index i+1. 

* If Pf is  ce [log] , Pf', then we generate log auto, followed by the sequence Pf' with index i 

* If Pf is ce @ ce' [res] , Pf', then we generate res $holei, followed by the sequence for Pf' with index i+1. 

* If Pf is (y) . ce [res], Pf', then we generate do y = $holei, followed by the sequence for Pf' with index i+1. 

Given a hole $hole in a refined context, we generate a sequence seq from $hole and Pf, starting at index 1, and then return (seq). 




