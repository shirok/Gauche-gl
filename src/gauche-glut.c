/*
 * gauche-glut.c - Gauche GLUT binding
 *
 *   Copyright (c) 2001-2012  Shiro Kawai  <shiro@acm.org>
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

#if MacOSX
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include "gauche-glut.h"

extern void Scm_Init_glut_lib(ScmModule *mod);

/*================================================================
 * Glut font
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GlutFontClass, NULL);

static ScmObj makeGlutFont(void *ptr)
{
    ScmGlutFont *gf = SCM_NEW(ScmGlutFont);
    SCM_SET_CLASS(gf, SCM_CLASS_GLUT_FONT);
    gf->font = ptr;
    return SCM_OBJ(gf);
}

/*================================================================
 * Callback support.
 *
 * Glut callbacks are associated to the "current window".
 * unfortunately the callback interface doesn't allow us
 * to pass extra data pointer, so our C callback routine
 * doesn't know which Scheme closure to be called.  We maintain
 * that information in our table.
 *
 * TODO: We don't want to use Scm_ApplyRec, for we need to cons
 * the arguments (display is ok, but motion and passiveMotion generates
 * garbages which will eventurally trigger GC.)  Rewrite *_cb functions
 * after we implement Scm_ApplyRec0, Scm_ApplyRec1, .., etc. in
 * Gauche core.
 */

static ScmObj ScmGlutCallbackTable = SCM_UNDEFINED; /* set by init routine */

static ScmObj get_callback(int type)
{
    int win = glutGetWindow();
    ScmObj entry = Scm_HashTableRef(SCM_HASH_TABLE(ScmGlutCallbackTable),
                                    SCM_MAKE_INT(win), SCM_FALSE);
    SCM_ASSERT(type >= 0 && type < SCM_GLUT_NUM_WINDOW_CBS);
    if (SCM_VECTORP(entry)) {
        return SCM_VECTOR_ELEMENT(entry, type);
    } else {
        return SCM_FALSE;
    }
}

#define define_callback(name, num, arglist, args)                       \
    static void SCM_CPP_CAT(name, _cb) arglist                          \
    {                                                                   \
        ScmObj cb = get_callback(SCM_CPP_CAT(SCM_GLUT_CB_, num));       \
        if (!SCM_FALSEP(cb)) {                                          \
            Scm_ApplyRec(cb, args);                                     \
        }                                                               \
    }
    
define_callback(display, DISPLAY, (void), SCM_NIL)


define_callback(overlay_display, OVERLAY_DISPLAY, (void), SCM_NIL)
define_callback(reshape, RESHAPE, (int w, int h),
                SCM_LIST2(SCM_MAKE_INT(w), SCM_MAKE_INT(h)));
define_callback(keyboard, KEYBOARD, (unsigned char key, int w, int h),
                SCM_LIST3(SCM_MAKE_INT(key), SCM_MAKE_INT(w), SCM_MAKE_INT(h)))
define_callback(keyboard_up, KEYBOARD_UP, (unsigned char key, int w, int h),
                SCM_LIST3(SCM_MAKE_INT(key), SCM_MAKE_INT(w), SCM_MAKE_INT(h)))
define_callback(mouse, MOUSE, (int button, int state, int x, int y),
                SCM_LIST4(SCM_MAKE_INT(button), SCM_MAKE_INT(state),
                          SCM_MAKE_INT(x), SCM_MAKE_INT(y)))
define_callback(motion, MOTION, (int x, int y),
                SCM_LIST2(SCM_MAKE_INT(x), SCM_MAKE_INT(y)))
define_callback(passive_motion, PASSIVE_MOTION, (int x, int y),
                SCM_LIST2(SCM_MAKE_INT(x), SCM_MAKE_INT(y)))
define_callback(visibility, VISIBILITY, (int state),
                SCM_LIST1(SCM_MAKE_INT(state)))
define_callback(entry, ENTRY, (int state),
                SCM_LIST1(SCM_MAKE_INT(state)))
define_callback(special, SPECIAL, (int key, int w, int h),
                SCM_LIST3(SCM_MAKE_INT(key), SCM_MAKE_INT(w), SCM_MAKE_INT(h)))
define_callback(special_up, SPECIAL_UP, (int key, int w, int h),
                SCM_LIST3(SCM_MAKE_INT(key), SCM_MAKE_INT(w), SCM_MAKE_INT(h)))
define_callback(spaceball_motion, SPACEBALL_MOTION, (int x, int y, int z),
                SCM_LIST3(SCM_MAKE_INT(x), SCM_MAKE_INT(y), SCM_MAKE_INT(z)))
define_callback(spaceball_rotate, SPACEBALL_ROTATE, (int x, int y, int z),
                SCM_LIST3(SCM_MAKE_INT(x), SCM_MAKE_INT(y), SCM_MAKE_INT(z)))
define_callback(spaceball_button, SPACEBALL_BUTTON, (int button, int state),
                SCM_LIST2(SCM_MAKE_INT(button), SCM_MAKE_INT(state)))
define_callback(button_box, BUTTON_BOX, (int button, int state),
                SCM_LIST2(SCM_MAKE_INT(button), SCM_MAKE_INT(state)))
define_callback(dials, DIALS, (int dial, int value),
                SCM_LIST2(SCM_MAKE_INT(dial), SCM_MAKE_INT(value)))
define_callback(tablet_motion, TABLET_MOTION, (int x, int y),
                SCM_LIST2(SCM_MAKE_INT(x), SCM_MAKE_INT(y)))
define_callback(tablet_button, TABLET_BUTTON,
                (int button, int state, int x, int y),
                SCM_LIST4(SCM_MAKE_INT(button),
                          SCM_MAKE_INT(state),
                          SCM_MAKE_INT(x),
                          SCM_MAKE_INT(y)))
define_callback(menu_status, MENU_STATUS, (int status, int x, int y),
                SCM_LIST3(SCM_MAKE_INT(status),
                          SCM_MAKE_INT(x), SCM_MAKE_INT(y)))
define_callback(window_status, WINDOW_STATUS, (int status),
                SCM_LIST1(SCM_MAKE_INT(status)))
define_callback(joystick, JOYSTICK, (unsigned int mask, int x, int y, int z),
                SCM_LIST4(SCM_MAKE_INT(mask),
                          SCM_MAKE_INT(x), SCM_MAKE_INT(y), SCM_MAKE_INT(z)))

/* global callbacks */
static ScmObj idle_closure = SCM_FALSE;

static void idle_cb(void)
{
    if (!SCM_FALSEP(idle_closure)) {
        Scm_ApplyRec(idle_closure, SCM_NIL);
    }
}

static ScmObj timer_closure = SCM_FALSE;

static void timer_cb(int value)
{
    if (!SCM_FALSEP(timer_closure)) {
        Scm_ApplyRec(timer_closure, SCM_LIST1(Scm_MakeInteger(value)));
    }
}


/* NB: these functions are new addition by freeglut.  we provide
   dummy functions for older versions. */
#if !(GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
static void glutKeyboardUpFunc(void (*fn)(unsigned char, int, int))
{
    Scm_Warn("glutKeyboardUpFunc unsupported in this version of GLUT");
}
static void glutSpecialUpFunc(void (*fn)(int, int, int))
{
    Scm_Warn("glutSpecialUpFunc unsupported in this version of GLUT");
}
static void glutJoystickFunc(void (*fn)(unsigned int, int, int, int),
                             int interval)
{
    Scm_Warn("glutJoystickFunc unsupported in this version of GLUT");
}
static void glutWindowStatusFunc(void (*fn)(unsigned int, int, int, int))
{
    Scm_Warn("glutWindowStatusFunc unsupported in this version of GLUT");
}
#endif


#define define_registrar(glutfn, cbname)                                \
    static void SCM_CPP_CAT(register_, cbname)(int flag, int xtra)      \
    {                                                                   \
        if (flag) {                                                     \
            glutfn(SCM_CPP_CAT(cbname, _cb));                           \
        } else {                                                        \
            glutfn(NULL);                                               \
        }                                                               \
    }

define_registrar(glutDisplayFunc, display)
define_registrar(glutOverlayDisplayFunc, overlay_display)
define_registrar(glutReshapeFunc, reshape)
define_registrar(glutKeyboardFunc, keyboard)
define_registrar(glutKeyboardUpFunc, keyboard_up)
define_registrar(glutMouseFunc, mouse)
define_registrar(glutMotionFunc, motion)
define_registrar(glutPassiveMotionFunc, passive_motion)
define_registrar(glutVisibilityFunc, visibility)
define_registrar(glutEntryFunc, entry)
define_registrar(glutSpecialFunc, special)
define_registrar(glutSpecialUpFunc, special_up)
define_registrar(glutSpaceballMotionFunc, spaceball_motion)
define_registrar(glutSpaceballRotateFunc, spaceball_rotate)
define_registrar(glutSpaceballButtonFunc, spaceball_button)
define_registrar(glutButtonBoxFunc, button_box)
define_registrar(glutDialsFunc, dials)
define_registrar(glutTabletMotionFunc, tablet_motion)
define_registrar(glutTabletButtonFunc, tablet_button)
define_registrar(glutMenuStatusFunc, menu_status)
define_registrar(glutWindowStatusFunc, window_status)

/* joystick fn is a bit different */
static void register_joystick(int flag, int interval)
{
    if (flag) {
        glutJoystickFunc(joystick_cb, interval);
    } else {
        glutJoystickFunc(NULL, interval);
    }
}


/* NB: order must match SCM_GLUT_CB_* enums */
static void (*registrars[])(int flag, int xtra) = {
    register_display,
    register_overlay_display,
    register_reshape,
    register_keyboard,
    register_mouse,
    register_motion,
    register_passive_motion,
    register_visibility,
    register_entry,
    register_special,
    register_spaceball_motion,
    register_spaceball_rotate,
    register_spaceball_button,
    register_button_box,
    register_dials,
    register_tablet_motion,
    register_tablet_button,
    register_menu_status,
    register_window_status,
    register_keyboard_up,
    register_special_up,
    register_joystick,
};

/*
 * External entry to manage registering callbacks
 * 'xtra1' and 'xtra2' are ignored by most callbacks; only the two callbacks
 * use them:
 *   glutTimerFunc: xtra1 for millliseconds, xtra2 for value
 *   glutJoystickFunc: xtra1 for interval
 */
void Scm_GlutRegisterCallback(int type, ScmObj closure, int xtra1, int xtra2)
{
    SCM_ASSERT(type >= 0 && type < SCM_GLUT_NUM_CBS);
    if (type < SCM_GLUT_NUM_WINDOW_CBS) {
        int win = glutGetWindow();
        ScmObj entry = Scm_HashTableRef(SCM_HASH_TABLE(ScmGlutCallbackTable),
                                        SCM_MAKE_INT(win), SCM_FALSE);
        
        if (SCM_EQ(entry, SCM_FALSE)) {
            entry = Scm_MakeVector(SCM_GLUT_NUM_WINDOW_CBS, SCM_FALSE);
            Scm_HashTableSet(SCM_HASH_TABLE(ScmGlutCallbackTable),
                             SCM_MAKE_INT(win), entry, 0);
        }
        SCM_VECTOR_ELEMENT(entry, type) = closure;
        registrars[type](!SCM_FALSEP(closure), xtra1);
    } else if (type == SCM_GLUT_CB_IDLE) {
        idle_closure = closure;
        if (SCM_FALSEP(closure)) {
            glutIdleFunc(NULL);
        } else {
            glutIdleFunc(idle_cb);
        }
    } else {
        timer_closure = closure;
        if (!SCM_FALSEP(closure)) {
            glutTimerFunc(xtra1, timer_cb, xtra2);
        }
    }
}

/*================================================================
 * Initialization
 */
void Scm_Init_libgauche_glut(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(libgauche_glut);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl.glut", TRUE));
    Scm_Init_glut_lib(mod);

    /* Callback table */
    ScmGlutCallbackTable = Scm_MakeHashTableSimple(SCM_HASH_EQV, 0);
    
    /* Glut built-in fonts */
#define DEFFONT(name) Scm_DefineConst(mod, SCM_SYMBOL(SCM_INTERN(#name)), makeGlutFont(name))
    /* Stroke font constants (use these in GLUT program). */
    DEFFONT(GLUT_STROKE_ROMAN);
    DEFFONT(GLUT_STROKE_MONO_ROMAN);

    /* Bitmap font constants (use these in GLUT program). */
    DEFFONT(GLUT_BITMAP_9_BY_15);
    DEFFONT(GLUT_BITMAP_8_BY_13);
    DEFFONT(GLUT_BITMAP_TIMES_ROMAN_10);
    DEFFONT(GLUT_BITMAP_TIMES_ROMAN_24);
#if (GLUT_API_VERSION >= 3)
    DEFFONT(GLUT_BITMAP_HELVETICA_10);
    DEFFONT(GLUT_BITMAP_HELVETICA_12);
    DEFFONT(GLUT_BITMAP_HELVETICA_18);
#endif
}
