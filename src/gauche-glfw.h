/*
 * gauche-glfw.h - Gauche GLFWT binding
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

#ifndef GAUCHE_GLFW_H
#define GAUCHE_GLFW_H

#include <GLFW/glfw3.h>

SCM_EXTERN ScmClass *ScmGlfwWindowClass; /* foreign pointer class */
#define SCM_GLFW_WINDOW_P(obj) SCM_ISA(obj, ScmGlfwWindowClass)
#define SCM_GLFW_WINDOW(obj)   SCM_FOREIGN_POINTER_REF(GLFWwindow*, obj)

SCM_EXTERN GLFWwindow *Scm_CreateGlfwWindow(int, int, const char *,
                                            GLFWmonitor*, GLFWwindow*);
SCM_EXTERN ScmObj Scm_MakeGlfwWindow(GLFWwindow*);
SCM_EXTERN void Scm_GlfwWindowDestroy(ScmObj);

SCM_EXTERN ScmClass *ScmGlfwMonitorClass; /* foreign pointer class */
#define SCM_GLFW_MONITOR_P(obj) SCM_ISA(obj, ScmGlfwMonitorClass)
#define SCM_GLFW_MONITOR(obj)   SCM_FOREIGN_POINTER_REF(GLFWmonitor*, obj)

SCM_EXTERN ScmObj Scm_MakeGlfwMonitor(GLFWmonitor*);

SCM_EXTERN ScmClass *ScmGlfwVidmodeClass; /* foreign pointer class */
#define SCM_GLFW_VIDMODE_P(obj) SCM_ISA(obj, ScmGlfwVidmodeClass)
#define SCM_GLFW_VIDMODE(obj)   SCM_FOREIGN_POINTER_REF(GLFWvidmode*, obj)

/* always copy the source */
SCM_EXTERN ScmObj Scm_MakeGlfwVidmode(const GLFWvidmode*);

/* 
 * This structure is associated to each window.
 */
typedef struct ScmGlfwWindowDataRec {
    ScmObj pos;       /* GLFWwindow*, int, int */
    ScmObj size;      /* GLFWwindow*, int, int */
    ScmObj close;     /* GLFWwindow* */
    ScmObj refresh;   /* GLFWwindow* */
    ScmObj focus;     /* GLFWwindow*, int */
    ScmObj iconify;   /* GLFWwindow*, int */
    ScmObj maximize;  /* GLFWwindow*, int */  // from 3.3
    ScmObj framesize; /* GLFWwindow*, int, int */
    ScmObj scale;     /* GLFWwindow*, float, float */ // from 3.3
} ScmGlfwWindowData;

SCM_EXTERN ScmGlfwWindowData *Scm_GlfwGetWindowData(GLFWwindow *);

SCM_EXTERN ScmObj Scm_SetGlfwErrorCallback(ScmObj proc);

#endif /*GAUCHE_GLFW_H*/
