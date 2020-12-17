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
 * windowdata management
 */

/* We keep windowdata in GLFWwindow's user pointer, but also need to
   store it in a global hashtable to prevent it (and callbacks) from
   being GC-ed. */

static ScmInternalMutex wtab_mutex;
static ScmHashCore wtab;

static void init_wtab()
{
    SCM_INTERNAL_MUTEX_INIT(wtab_mutex);
    Scm_HashCoreInitSimple(&wtab, SCM_HASH_EQ, 0, NULL);
}

static ScmGlfwWindowData *ensure_window_data(GLFWwindow *w)
{
    SCM_INTERNAL_MUTEX_LOCK(wtab_mutex);
    ScmDictEntry *e = Scm_HashCoreSearch(&wtab,
                                         (intptr_t)w,
                                         SCM_DICT_CREATE);
    SCM_INTERNAL_MUTEX_UNLOCK(wtab_mutex);
    if (SCM_DICT_VALUE(e) == 0) {
        ScmGlfwWindowData *data = SCM_NEW(ScmGlfwWindowData);
        for (int i=0; i<SCM_GLFW_NUM_WINDOW_CALLBACKS; i++) {
            data->window_cbs[i] = SCM_FALSE;
        }
        e->value = (intptr_t)data;
        return data;
    } else {
        return (ScmGlfwWindowData*)SCM_DICT_VALUE(e);
    }
}

static void discard_window_data(GLFWwindow *w)
{
    SCM_INTERNAL_MUTEX_LOCK(wtab_mutex);
    Scm_HashCoreSearch(&wtab, (intptr_t)w, SCM_DICT_DELETE);
    SCM_INTERNAL_MUTEX_UNLOCK(wtab_mutex);
}

ScmGlfwWindowData *Scm_GlfwGetWindowData(GLFWwindow *w)
{
    void *p = glfwGetWindowUserPointer(w);
    if (p != NULL) return (ScmGlfwWindowData*)p;
    ScmGlfwWindowData *d = ensure_window_data(w);
    glfwSetWindowUserPointer(w, d);
    return d;
}
    

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
        glfwSetWindowUserPointer(w, NULL);
        discard_window_data(w);
        glfwDestroyWindow(w);
    }
}

/* constructor */
GLFWwindow *Scm_CreateGlfwWindow(int width, int height, const char *title,
                                 GLFWmonitor *monitor,
                                 GLFWwindow *share)
{
    GLFWwindow *w = glfwCreateWindow(width, height, title, monitor, share);
    if (w != NULL) {
        (void)Scm_GlfwGetWindowData(w); /* attach WindowData */
        Scm__SetupWindowCallbacks(w);
    }
    return w;
}

/* This is for type conversion */
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
 * GLFWMonitor
 */

ScmClass *ScmGlfwMonitorClass;

static void glfw_monitor_print(ScmObj obj, ScmPort *sink, 
                               ScmWriteContext *m SCM_UNUSED)
{
    Scm_Printf(sink, "#<glfw-monitor %s>", 
               glfwGetMonitorName(SCM_GLFW_MONITOR(obj)));
}

ScmObj Scm_MakeGlfwMonitor(GLFWmonitor *m)
{
    return Scm_MakeForeignPointer(ScmGlfwMonitorClass, m);
}

/*================================================================
 * GLFWcursor
 */

ScmClass *ScmGlfwCursorClass;

/* user created cursor is marked with 'user-created attribute. */
static ScmObj sym_user_created;

static void glfw_cursor_print(ScmObj obj, ScmPort *sink, 
                              ScmWriteContext *m SCM_UNUSED)
{
    if (Scm_ForeignPointerInvalidP(SCM_FOREIGN_POINTER(obj))) {
        Scm_Printf(sink, "#<glfw-cursor (destroyed)>");
    } else {
        Scm_Printf(sink, "#<glfw-cursor %p>", SCM_GLFW_CURSOR(obj));
    }
}

static void glfw_cursor_cleanup(ScmObj obj)
{
    ScmForeignPointer *p = SCM_FOREIGN_POINTER(obj);
    if (Scm_ForeignPointerInvalidP(p)) return;

    ScmObj user_created = Scm_ForeignPointerAttrGet(p, sym_user_created,
                                                    SCM_FALSE);
    if (!SCM_FALSEP(user_created)) {
        GLFWcursor *w = SCM_GLFW_CURSOR(obj);
        if (w != NULL) {
            glfwDestroyCursor(w);
        }
    }
    Scm_ForeignPointerInvalidate(p);
}

ScmObj Scm_MakeGlfwCursor(GLFWcursor *m)
{
    return Scm_MakeForeignPointer(ScmGlfwCursorClass, m);
}

void Scm_GlfwCursorDestroy(ScmObj cursor)
{
    if (!SCM_GLFW_CURSOR_P(cursor)) {
        SCM_TYPE_ERROR(cursor, "<glfw-cursor>");
    }
    glfw_cursor_cleanup(cursor);
}


/*================================================================
 * GLFWVidmode
 */

ScmClass *ScmGlfwVidmodeClass;

static void glfw_vidmode_print(ScmObj obj, ScmPort *sink, 
                               ScmWriteContext *m SCM_UNUSED)
{
    Scm_Printf(sink, "#<glfw-vidmode %p>", SCM_GLFW_VIDMODE(obj));
}

/* it's a struct owned by GFLW, so we always copy it. */
ScmObj Scm_MakeGlfwVidmode(const GLFWvidmode *m)
{
    GLFWvidmode *z = SCM_NEW_ATOMIC(GLFWvidmode);
    *z = *m;
    return Scm_MakeForeignPointer(ScmGlfwVidmodeClass, z);
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
    ScmGlfwMonitorClass = 
        Scm_MakeForeignPointerClass(mod, "<glfw-monitor>",
                                    glfw_monitor_print,
                                    NULL,
                                    SCM_FOREIGN_POINTER_KEEP_IDENTITY);
    ScmGlfwCursorClass = 
        Scm_MakeForeignPointerClass(mod, "<glfw-cursor>",
                                    glfw_cursor_print,
                                    NULL,
                                    SCM_FOREIGN_POINTER_KEEP_IDENTITY);
    ScmGlfwVidmodeClass = 
        Scm_MakeForeignPointerClass(mod, "<glfw-vidmode>",
                                    glfw_vidmode_print,
                                    NULL,
                                    SCM_FOREIGN_POINTER_KEEP_IDENTITY);

    sym_user_created = SCM_INTERN("user-created");
    init_wtab();
    Scm_Init_glfw_lib(mod);
}

