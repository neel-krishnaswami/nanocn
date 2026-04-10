Write a tool to read a collection of Ocaml .cmt files, and write 2 information files. 

In the following format description:

  - "name" means the fully-qualified name of a toplevel Ocaml function
  - "position" means a file:name:column 


- locations.txt:

  The file should contain all the toplevel functions in the program

  name : position : position 

- callsites.txt 

  Each line of the file should have the format: 

  name name position 

  interpreted so that "M.f N.L.g foo.ml:3:5" means that M.f calls N.L.g in the file 
  foo.ml at line 3, starting in column 5. 

