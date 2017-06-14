# Bi_directional_diagrams_gui



This is a GUI that allows not only writing diagrams code and immediately seeing the resulting image,
but also directly manipulating the image (e.g. by dragging or resizing things) and have it update the code.

As an example of why one would need such a tool, consider the scenario where one resizes a circle through
dragging on the edge. It would be nice if one could have an idea of what the new radius of the circle is after
dragging and maybe even better, if he/she could bring it back to what it was before dragging without having to drag again
and approximate the size of the original circle. This GUI allows one to do exactly that. Update the code and see the change to the diagram or modify the diagram and see the change in the code all in "real-time".


Using Functional Reactive Programming (FRP) principles, the GUI is being developped through
Threepenny-gui by using its Reactive.Threepenny library. This not only provides a kind of automation that would
otherwise be almost impossible to achieve but also it makes the code much cleaner and thus easier to edit.(compare with the
original version of the program without the reactive component)

As of now, the GUI is using a tiny language built on top of diagrams, but as the development of the GUI continues
so will the language become richer.
