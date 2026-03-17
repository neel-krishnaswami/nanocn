I want to introduce a backend which takes a surface program and produces an HTML rendering of it. 

There should take the type- and context-annotated term we get after typechecking/elaboration, 
and render it to a webpage. 

(1) a main panel where the code can be looked at. The code should be nicely indented and 
    colorized. If the user hovers over an identifier, its type should be displayed in a 
    tooltip. 

(2) To the right of the code, there should be a context display, which lists all of the free 
    variables and their types, for the current subterm. The current scope is: 

    - if the user has clicked a position in the source code, it should be the context of the 
      smallest enclosing subterm. There should be a visual indication of where the position is,
      like changing the background color of that term. 

    - if the user has selected a region, it should be the context of the smallest subterm 
      enclosing the selected area. The background color of the whole selected subterm should 
      change. 

(3) In addition, at the bottom of the right panel, the type of the current subterm should be
    rendered. 