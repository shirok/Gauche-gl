/*
 * gauche-glut.h - Gauche GLUT binding
 *
 *   Copyright (c) 2001-2014  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_GLUT_H
#define GAUCHE_GLUT_H

/* opaque pointer for glut font */
typedef struct ScmGlutFontRec {
    SCM_HEADER;
    void *font;
} ScmGlutFont;

SCM_CLASS_DECL(Scm_GlutFontClass);
#define SCM_CLASS_GLUT_FONT   (&Scm_GlutFontClass)
#define SCM_GLUT_FONT_P(obj)  (SCM_XTYPEP(obj, SCM_CLASS_GLUT_FONT))
#define SCM_GLUT_FONT(obj)    ((ScmGlutFont*)(obj))

/* glut callback table */
enum {
    /* per-window callbacks */
    SCM_GLUT_CB_DISPLAY,
    SCM_GLUT_CB_OVERLAY_DISPLAY,
    SCM_GLUT_CB_RESHAPE,
    SCM_GLUT_CB_KEYBOARD,
    SCM_GLUT_CB_MOUSE,
    SCM_GLUT_CB_MOTION,
    SCM_GLUT_CB_PASSIVE_MOTION,
    SCM_GLUT_CB_VISIBILITY,
    SCM_GLUT_CB_ENTRY,
    SCM_GLUT_CB_SPECIAL,
    SCM_GLUT_CB_SPACEBALL_MOTION,
    SCM_GLUT_CB_SPACEBALL_ROTATE,
    SCM_GLUT_CB_SPACEBALL_BUTTON,
    SCM_GLUT_CB_BUTTON_BOX,
    SCM_GLUT_CB_DIALS,
    SCM_GLUT_CB_TABLET_MOTION,
    SCM_GLUT_CB_TABLET_BUTTON,
    SCM_GLUT_CB_MENU_STATUS,
    SCM_GLUT_CB_WINDOW_STATUS,  /* freeglut addition (glut API version 4) */
    SCM_GLUT_CB_KEYBOARD_UP,    /* freeglut addition (glut API version 4) */
    SCM_GLUT_CB_SPECIAL_UP,     /* freeglut addition (glut API version 4) */
    SCM_GLUT_CB_JOYSTICK,       /* freeglut addition (glut API version 4) */

    SCM_GLUT_NUM_WINDOW_CBS,    /* marker */

    /* global callbacks */
    SCM_GLUT_CB_IDLE = SCM_GLUT_NUM_WINDOW_CBS,
    SCM_GLUT_CB_TIMER,

    SCM_GLUT_NUM_CBS
};

extern void Scm_GlutRegisterCallback(int type, ScmObj closure,
                                     int xtra1, int xtra2);
    
#endif /*GAUCHE_GLUT_H */

