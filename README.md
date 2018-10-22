# A Proof of Concept for a Bi-directional GUI for Rendering Diagrams


# Build

- clone the project
- `cd bigui`
- `stack build`
- `stack exec bigui`

# GUI Language

## Primitives

This GUI uses a small language developed with it to ease testing out of ideas.
The language comprises of three primitives:

- **circle**
- **triangle**
- **polygon n** (where n is the number of sides of a given polygon)

## Functions

The GUI language supports two types of functions. Functions to *transform* a given
diagram, and functions to combine smaller diagrams into a bigger one.

### Transformations:
- **scale d** This function scales a given diagram by the given size d.

     e.g: `scale 1.5 circle`
- **translate (x,y)** This function translates a given diagram by the vector (x,y).

     e.g: `translate (1.0, 1.0) triangle`
- **rotate a** This function rotates a given function by the given angle a in degrees.

     e.g: `rotate 30.0 triangle`

### Combinations:
- **atop d1 d2** This function puts diagrams d1 and d2 on top of each other to create a new diagram.

     e.g: `atop circle polygon 7`
- **iterate n transformation d** This function applies the given transformation on d n times and puts the resulting diagrams together to create a new diagram.

     e.g `iterate 5 scale 1.5 atop circle triangle`

## Focus in language's AST

The current focus in the AST on which transformations are to be made is indicated in the editor side of
the GUI by `==>`. The sub diagram that corresponds to node on which is the current focus is seen to be bolder than the rest of the diagram.

## Other features

* The GUI language supports the definition of variables. Variables can be defined before one starts definining the diagram. They have to be enclosed by #.
       e.g:
       ```
       #
       a = circle;
       b = triangle;
       #
       atop
          a
          b
       ```
* The **iterate** transformation function has a variant. if defined as **iterate n transformation [/i] d**, The interpretation is that node i has been disconnected from the rest of the nodes.

  e.g: `iterate 3 scale 1.5 [/2] circle` will generate 2 circles because the second circle in the iteration has been removed.

## Diagram side features
  * In the diagram side of the GUI, you can drag and drop a diagram in focus, and a corresponding transformation vector will be automatically calculated in the editor side.

  * You can click anywhere, and a copy of the diagram in focus will be inserted at the location where
  you clicked. (It doesn't actually work like this one. If you click a diagram will be created but not at the exact same location where the click was made.)

  * The GUI has a text field that can be used to split a diagram from an iterate transformation node.
  If a valid number is entered into the textfield, a corresponding diagram in the iteration will be detached and corresponding code will be generated.

## Additional implemantation remarks

The GUI is implemented using the concept of *functional reactive programming*. Moreover, A zipper data structure is implemented to simulate moving around the AST of the program.

