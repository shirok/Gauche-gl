/*
 * gauche-glfw.c - Gauche GLFW binding
 *
 *   Copyright (c) 2020  Shiro Kawai  <shiro@acm.org>
 * 
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 * 
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <gauche.h>
#include <gauche/extend.h>
#include "gauche-glfw.h"

extern void Scm_Init_glfw_lib(ScmModule *mod);

/*================================================================
 * GLFWwindow
 */
ScmClass *ScmGlfwWindowClass;

static void glfw_window_print(ScmObj obj, ScmPort *sink, 
                              ScmWriteContext *m SCM_UNUSED)
{
    if (Scm_ForeignPointerInvalidP(SCM_FOREIGN_POINTER(obj))) {
        Scm_Printf(sink, "#<glfw-window (destroyed)>");
    } else {
        Scm_Printf(sink, "#<glfw-window %p>", SCM_GLFW_WINDOW(obj));
    }
}

static void glfw_window_cleanup(ScmObj obj)
{
    if (Scm_ForeignPointerInvalidP(SCM_FOREIGN_POINTER(obj))) return;
    
    GLFWwindow *w = SCM_GLFW_WINDOW(obj);
    if (w != NULL) {
        Scm_ForeignPointerInvalidate(SCM_FOREIGN_POINTER(obj));
        glfwDestroyWindow(w);
    }
}

ScmObj Scm_MakeGlfwWindow(GLFWwindow *w)
{
    return Scm_MakeForeignPointer(ScmGlfwWindowClass, w);
}

void Scm_GlfwWindowDestroy(ScmObj window)
{
    if (!SCM_GLFW_WINDOW_P(window)) {
        SCM_TYPE_ERROR(window, "<glfw-window>");
    }
    glfw_window_cleanup(window);
}

/*================================================================
 * Initialization
 */
void Scm_Init_libgauche_glfw(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(libgauche_glfw);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl.glfw", TRUE));

    ScmGlfwWindowClass = 
        Scm_MakeForeignPointerClass(mod, "<glfw-window>",
                                    glfw_window_print,
                                    glfw_window_cleanup,
                                    SCM_FOREIGN_POINTER_KEEP_IDENTITY);
    Scm_Init_glfw_lib(mod);
}

