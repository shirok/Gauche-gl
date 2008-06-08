/*
 * gauche-glut.h - Gauche GLUT binding
 *
 *  Copyright (c) 2001-2008  Shiro Kawai  <shiro@acm.org>
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: gauche-glut.h,v 1.3 2008-06-04 11:50:21 shirok Exp $
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

