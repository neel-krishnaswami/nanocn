# Plan for editor support for nanoCN

I want good editor support for nanocn. In particular, I want a high-quality Emacs mode
for it. 

To do so, we will need: 

1. A tree-sitter grammar for nanocn. This should be based on the grammar in parser.mly, 
   and generate a dynamically loadable object file that Emacs can use. 

2. An LSP server for nanocn. The nanocn compiler should have a back end which generates
   information for the language server, which it can re-run on each save of the file. 

   The compiler should be updated so that files are checked on a per-definition basis, 
   and a type or parse error in one definition does not break checking of the rest of 
   the file. 

   Also, the LSP server should check SMT queries asynchronously, so that editor services
   are not held up by waiting for the SMT solver. Instead, as the constraints are solved,
   the server should inform the client that definitions are good, and signal an error 
   when they are violated. 

3. An emacs mode which using the tree-sitter plugin and the LSP server. One of the
   features it should offer is the ability to list the current context at point. 
