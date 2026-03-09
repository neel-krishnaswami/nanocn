This is a small implementation of a CN+Fulminate style system. 

1. The core language syntax is based on Jad Ghalayini's functional syntax for SSA, with the idea that this will make it easy to eventually convert it to work over real LLVM/MLIR IR, while letting us work in a language with sane scoping and substitution principles. 
2. The refinement layer is inspired by Dhruv Makwana's PhD thesis. This has multiple levels:
   * Pure ghost terms, corresponding to SMT terms and formulas, including recursive functions
   * Predicate expressions, which are monadically-typed ghost terms, embodying separation logic formulas, including recursive predicates
   * Resource terms, which are proof terms for resource predicates
   * Commands, which manipulate the ghost state (eg, unfolding recursive predicates and functions, extracting and inserting elements of arrays, applying lemmas).
   * Finally, primitive calls in the core language are extended to allow function calls with ghost arguments/returns.
   * Commands and ghost args/returns are extensions of the core, but are eraseable. 
3. There is a minimal SMT-based typechecker for the language. Many of CN's quality-of-life features (such as automatic unfolding of predicates) will be omitted, because the plan is to get Claude to write the annotations (and this implementation, for that matter). Probably we will also require annotations at all control-flow mergers, too, so that we don't have to separately recheck each control-flow path the way CN does now.  
4. There is an interpreter for this language, which has two modes.
   * In mode 1, it just ignores the ghost state and runs the program.
   * In mode 2, all the ghost state is evaluated at runtime alongside the program a la Fulminate.
   
Another difference from Fulminate is that we will track *all* of the abstract resources of the program (i.e., the resource context) symbolically. In Fulminate, each byte of memory is owned by a stack frame. Here, each byte of memory will be owned by a *predicate call `P(t1,...,tn)`*, where the `P` is a predicate name and the `ti` are concrete ground terms. 

The reason for this is that we want all of the proof-manipulating commands to have an operational semantics, so that we can turn static errors into runtime errors, and use  concrete program states to help figure out what is going wrong with a proof, using ideas from CEGAR/CEGIS. The idea is that the results of these analyses can be invoked and studied both by programmers and by LLMs like Claude (maybe as a "skill"), and my conjecture is that informational mechanisms which are good for humans will also be good for machines. 
   
     
