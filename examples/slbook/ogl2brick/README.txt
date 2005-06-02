LAST UPDATE TO THIS FILE

    18-Mar-2004

LICENSING INFORMATION

    See 3Dlabs-License.txt for license information

OVERVIEW

    This is a GLUT 3.7-based demo of the OpenGL Shading Language.  It
    includes a vertex shader/fragment shader pair that procedurally
    generates a brick pattern. These shaders are the shaders that are
    described in Chapter 6 of the book, OpenGL Shading Language, published
    by Addison-Wesley. In addition, this demo includes the application
    source code that is included and described in Chapter 7 of this book.

    When launched, ogl2brick loads, compiles, and links the brick shaders,
    then installs them as part of current state. Keyboard commands for this
    demo are:

    b - Toggle among background clear colors
    q - Quit
    t - Toggle among models to render
    ? - Help
    <home>     - reset zoom and rotation
    <space> or <click>        - stop rotation
    <+>, <-> or <ctrl + drag> - zoom model
    <arrow keys> or <drag>    - rotate model

    Be sure to cycle through the different models (box, teapot, sphere,
    torus) using the "t" key in order to see how this shader looks on
    different models.

    The application prints various diagnostic and InfoLog messages to the
    console window when building and applying GLSL shaders.

    This application can be compiled and linked using the VisualStudio.net
    solution file that is included (ogl2brick.sln).
