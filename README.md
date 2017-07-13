# Bi_directional_diagrams_gui



This is a GUI that allows not only writing diagrams code and immediately seeing the resulting image,
but also directly manipulating the image (e.g. by dragging or resizing things) and have it update the code.


Using Functional Reactive Programming (FRP) principles, the GUI is being developped through
Threepenny-gui by using its Reactive.Threepenny library. This not only provides a kind of automation that would
otherwise be almost impossible to achieve but also it makes the code much cleaner and thus easier to edit.(compare with the
original version of the program without the reactive component)

As of now, the GUI is using a tiny language built on top of diagrams, but as the development of the GUI continues
so will the language become richer.
