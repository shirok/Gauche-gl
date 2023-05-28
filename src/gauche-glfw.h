/*
 * gauche-glfw.h - Gauche GLFW binding
 *
 *   Copyright (c) 2020-2023  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_GLFW_H
#define GAUCHE_GLFW_H

#include <GLFW/glfw3.h>

SCM_EXTERN ScmClass *Scm_GlfwWindowClass; /* foreign pointer class */
#define SCM_GLFW_WINDOW_P(obj) SCM_ISA(obj, Scm_GlfwWindowClass)
#define SCM_GLFW_WINDOW(obj)   SCM_FOREIGN_POINTER_REF(GLFWwindow*, obj)

SCM_EXTERN GLFWwindow *Scm_CreateGlfwWindow(int, int, const char *,
                                            GLFWmonitor*, GLFWwindow*);
SCM_EXTERN ScmObj Scm_MakeGlfwWindow(GLFWwindow*);
SCM_EXTERN void Scm_GlfwWindowDestroy(ScmObj);

SCM_EXTERN ScmClass *Scm_GlfwMonitorClass; /* foreign pointer class */
#define SCM_GLFW_MONITOR_P(obj) SCM_ISA(obj, Scm_GlfwMonitorClass)
#define SCM_GLFW_MONITOR(obj)   SCM_FOREIGN_POINTER_REF(GLFWmonitor*, obj)

SCM_EXTERN ScmObj Scm_MakeGlfwMonitor(GLFWmonitor*);

SCM_EXTERN ScmClass *Scm_GlfwCursorClass; /* foreign pointer class */
#define SCM_GLFW_CURSOR_P(obj) SCM_ISA(obj, Scm_GlfwCursorClass)
#define SCM_GLFW_CURSOR(obj)   SCM_FOREIGN_POINTER_REF(GLFWcursor*, obj)

SCM_EXTERN ScmObj Scm_MakeGlfwCursor(GLFWcursor*);
SCM_EXTERN void Scm_GlfwCursorDestroy(ScmObj);

/*
 * This structure is associated to each window.
 */

/* window callbacks */
enum {
    SCM_GLFW_POS_CALLBACK,       /* GLFWwindow*, int, int */
    SCM_GLFW_SIZE_CALLBACK,      /* GLFWwindow*, int, int */
    SCM_GLFW_CLOSE_CALLBACK,     /* GLFWwindow* */
    SCM_GLFW_REFRESH_CALLBACK,   /* GLFWwindow* */
    SCM_GLFW_FOCUS_CALLBACK,     /* GLFWwindow*, int */
    SCM_GLFW_ICONIFY_CALLBACK,   /* GLFWwindow*, int */
    SCM_GLFW_MAXIMIZE_CALLBACK,  /* GLFWwindow*, int */  // from 3.3
    SCM_GLFW_FRAMESIZE_CALLBACK, /* GLFWwindow*, int, int */
    SCM_GLFW_SCALE_CALLBACK,     /* GLFWwindow*, float, float */ // from 3.3

    SCM_GLFW_KEY_CALLBACK,       /* GLFWwindow*, int, int, int, int */
    SCM_GLFW_CHAR_CALLBACK,      /* GLFWwindow*, unsigned int */
    SCM_GLFW_BUTTON_CALLBACK,    /* GLFWwindow*, int, int, int  */
    SCM_GLFW_CURSORPOS_CALLBACK, /* GLFWwindow*, double, double */
    SCM_GLFW_ENTER_CALLBACK,     /* GLFWwindow*, int */
    SCM_GLFW_SCROLL_CALLBACK,    /* GLFWwindow*, double, double */
    SCM_GLFW_DROP_CALLBACK,      /* GLFWwindow*, int, const char*[] */
    /* we don't support CharModsCallback, for it will be removed */

    SCM_GLFW_NUM_WINDOW_CALLBACKS
};


typedef struct ScmGlfwWindowDataRec {
    ScmObj window_cbs[SCM_GLFW_NUM_WINDOW_CALLBACKS];
} ScmGlfwWindowData;

SCM_EXTERN void Scm__SetupWindowCallbacks(GLFWwindow *);
SCM_EXTERN ScmGlfwWindowData *Scm_GlfwGetWindowData(GLFWwindow *);

/*
 * Global callbacks
 */

enum {
    SCM_GLFW_ERROR_CALLBACK,
    SCM_GLFW_JOYSTICK_CALLBACK,

    SCM_GLFW_NUM_GLOBAL_CALLBACKS
};

#endif /*GAUCHE_GLFW_H*/
