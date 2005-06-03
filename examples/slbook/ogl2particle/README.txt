LAST UPDATE TO THIS FILE

    18-Mar-2004

LICENSING INFORMATION

    See 3Dlabs-License.txt for license information

OVERVIEW

    This is a GLUT 3.7-based demo of the OpenGL Shading Language.  It
    includes a vertex shader/fragment shader pair that uses a particle
    system to render a "confetti cannon" effect. These shaders are the
    shaders that are described in Chapter 13 of the book, OpenGL Shading
    Language, published by Addison-Wesley.

    When launched, ogl2particle loads, compiles, and links the particle
    shaders, then installs them as part of current state. Keyboard commands
    for this demo are:

    b - Toggle among background clear colors
    q - Quit
    ? - Help
    <home>     - reset zoom and rotation
    <space> or <click>        - stop rotation
    <+>, <-> or <ctrl + drag> - zoom model
    <arrow keys> or <drag>    - rotate model

    The application prints various diagnostic and InfoLog messages to the
    console window when building and applying GLSL shaders.

    This application can be compiled and linked using the VisualStudio.net
    solution file that is included (ogl2particle.sln).
