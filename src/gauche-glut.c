/*
 * gauche-glut.c - Gauche GLUT binding
 *
 *  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: gauche-glut.c,v 1.2 2001-09-29 20:54:16 shirok Exp $
 */

#include <gauche.h>
#include <GL/glut.h>
#include "gauche-glut.h"

extern void Scm_Init_glut_lib(ScmModule *mod);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_GlutFontClass, NULL);

static ScmObj makeGlutFont(void *ptr)
{
    ScmGlutFont *gf = SCM_NEW(ScmGlutFont);
    SCM_SET_CLASS(gf, SCM_CLASS_GLUT_FONT);
    gf->font = ptr;
    return SCM_OBJ(gf);
}

void Scm_Init_gauche_glut(void)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("gl.glut", TRUE));
    Scm_Init_glut_lib(mod);

    /* Glut constants. */
#define DEF(name)  SCM_DEFINE(mod, #name, Scm_MakeInteger(name))
#define DEFFONT(name) SCM_DEFINE(mod, #name, makeGlutFont(name))

    /* Display mode bit masks */
    DEF(GLUT_RGB);
    DEF(GLUT_RGBA);
    DEF(GLUT_INDEX);
    DEF(GLUT_SINGLE);
    DEF(GLUT_DOUBLE);
    DEF(GLUT_ACCUM);
    DEF(GLUT_ALPHA);
    DEF(GLUT_DEPTH);
    DEF(GLUT_STENCIL);
#if (GLUT_API_VERSION >= 2)
    DEF(GLUT_MULTISAMPLE);
    DEF(GLUT_STEREO);
#endif
#if (GLUT_API_VERSION >= 3)
    DEF(GLUT_LUMINANCE);
#endif

    /* Mouse */
    DEF(GLUT_LEFT_BUTTON);
    DEF(GLUT_MIDDLE_BUTTON);
    DEF(GLUT_RIGHT_BUTTON);
    DEF(GLUT_DOWN);
    DEF(GLUT_UP);

#if (GLUT_API_VERSION >= 2)
    /* Function keys */
    DEF(GLUT_KEY_F1);
    DEF(GLUT_KEY_F2);
    DEF(GLUT_KEY_F3);
    DEF(GLUT_KEY_F4);
    DEF(GLUT_KEY_F5);
    DEF(GLUT_KEY_F6);
    DEF(GLUT_KEY_F7);
    DEF(GLUT_KEY_F8);
    DEF(GLUT_KEY_F9);
    DEF(GLUT_KEY_F10);
    DEF(GLUT_KEY_F11);
    DEF(GLUT_KEY_F12);
    DEF(GLUT_KEY_LEFT);
    DEF(GLUT_KEY_UP);
    DEF(GLUT_KEY_RIGHT);
    DEF(GLUT_KEY_DOWN);
    DEF(GLUT_KEY_PAGE_UP);
    DEF(GLUT_KEY_PAGE_DOWN);
    DEF(GLUT_KEY_HOME);
    DEF(GLUT_KEY_END);
    DEF(GLUT_KEY_INSERT);
#endif

    /* Entry/exit  state. */
    DEF(GLUT_LEFT);
    DEF(GLUT_ENTERED);

    /* Menu usage  state. */
    DEF(GLUT_MENU_NOT_IN_USE);
    DEF(GLUT_MENU_IN_USE);

    /* Visibility  state. */
    DEF(GLUT_NOT_VISIBLE);
    DEF(GLUT_VISIBLE);

    /* Window status  state. */
    DEF(GLUT_HIDDEN);
    DEF(GLUT_FULLY_RETAINED);
    DEF(GLUT_PARTIALLY_RETAINED);
    DEF(GLUT_FULLY_COVERED);

    /* Color index component selection values. */
    DEF(GLUT_RED);
    DEF(GLUT_GREEN);
    DEF(GLUT_BLUE);

    /* Layers for use. */
    DEF(GLUT_NORMAL);
    DEF(GLUT_OVERLAY);

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

    /* glutGet parameters. */
    DEF(GLUT_WINDOW_X);
    DEF(GLUT_WINDOW_Y);
    DEF(GLUT_WINDOW_WIDTH);
    DEF(GLUT_WINDOW_HEIGHT);
    DEF(GLUT_WINDOW_BUFFER_SIZE);
    DEF(GLUT_WINDOW_STENCIL_SIZE);
    DEF(GLUT_WINDOW_DEPTH_SIZE);
    DEF(GLUT_WINDOW_RED_SIZE);
    DEF(GLUT_WINDOW_GREEN_SIZE);
    DEF(GLUT_WINDOW_BLUE_SIZE);
    DEF(GLUT_WINDOW_ALPHA_SIZE);
    DEF(GLUT_WINDOW_ACCUM_RED_SIZE);
    DEF(GLUT_WINDOW_ACCUM_GREEN_SIZE);
    DEF(GLUT_WINDOW_ACCUM_BLUE_SIZE);
    DEF(GLUT_WINDOW_ACCUM_ALPHA_SIZE);
    DEF(GLUT_WINDOW_DOUBLEBUFFER);
    DEF(GLUT_WINDOW_RGBA);
    DEF(GLUT_WINDOW_PARENT);
    DEF(GLUT_WINDOW_NUM_CHILDREN);
    DEF(GLUT_WINDOW_COLORMAP_SIZE);
#if (GLUT_API_VERSION >= 2)
    DEF(GLUT_WINDOW_NUM_SAMPLES);
    DEF(GLUT_WINDOW_STEREO);
#endif
#if (GLUT_API_VERSION >= 3)
    DEF(GLUT_WINDOW_CURSOR);
#endif
    DEF(GLUT_SCREEN_WIDTH);
    DEF(GLUT_SCREEN_HEIGHT);
    DEF(GLUT_SCREEN_WIDTH_MM);
    DEF(GLUT_SCREEN_HEIGHT_MM);
    DEF(GLUT_MENU_NUM_ITEMS);
    DEF(GLUT_DISPLAY_MODE_POSSIBLE);
    DEF(GLUT_INIT_WINDOW_X);
    DEF(GLUT_INIT_WINDOW_Y);
    DEF(GLUT_INIT_WINDOW_WIDTH);
    DEF(GLUT_INIT_WINDOW_HEIGHT);
    DEF(GLUT_INIT_DISPLAY_MODE);
#if (GLUT_API_VERSION >= 2)
    DEF(GLUT_ELAPSED_TIME);
#endif
#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
    DEF(GLUT_WINDOW_FORMAT_ID);
#endif

#if (GLUT_API_VERSION >= 2)
    /* glutDeviceGet parameters. */
    DEF(GLUT_HAS_KEYBOARD);
    DEF(GLUT_HAS_MOUSE);
    DEF(GLUT_HAS_SPACEBALL);
    DEF(GLUT_HAS_DIAL_AND_BUTTON_BOX);
    DEF(GLUT_HAS_TABLET);
    DEF(GLUT_NUM_MOUSE_BUTTONS);
    DEF(GLUT_NUM_SPACEBALL_BUTTONS);
    DEF(GLUT_NUM_BUTTON_BOX_BUTTONS);
    DEF(GLUT_NUM_DIALS);
    DEF(GLUT_NUM_TABLET_BUTTONS);
#endif
#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 13)
    DEF(GLUT_DEVICE_IGNORE_KEY_REPEAT);
    DEF(GLUT_DEVICE_KEY_REPEAT);
    DEF(GLUT_HAS_JOYSTICK);
    DEF(GLUT_OWNS_JOYSTICK);
    DEF(GLUT_JOYSTICK_BUTTONS);
    DEF(GLUT_JOYSTICK_AXES);
    DEF(GLUT_JOYSTICK_POLL_RATE);
#endif

#if (GLUT_API_VERSION >= 3)
    /* glutLayerGet parameters. */
    DEF(GLUT_OVERLAY_POSSIBLE);
    DEF(GLUT_LAYER_IN_USE);
    DEF(GLUT_HAS_OVERLAY);
    DEF(GLUT_TRANSPARENT_INDEX);
    DEF(GLUT_NORMAL_DAMAGED);
    DEF(GLUT_OVERLAY_DAMAGED);

#if (GLUT_API_VERSION >= 4 || GLUT_XLIB_IMPLEMENTATION >= 9)
    /* glutVideoResizeGet parameters. */
    DEF(GLUT_VIDEO_RESIZE_POSSIBLE);
    DEF(GLUT_VIDEO_RESIZE_IN_USE);
    DEF(GLUT_VIDEO_RESIZE_X_DELTA);
    DEF(GLUT_VIDEO_RESIZE_Y_DELTA);
    DEF(GLUT_VIDEO_RESIZE_WIDTH_DELTA);
    DEF(GLUT_VIDEO_RESIZE_HEIGHT_DELTA);
    DEF(GLUT_VIDEO_RESIZE_X);
    DEF(GLUT_VIDEO_RESIZE_Y);
    DEF(GLUT_VIDEO_RESIZE_WIDTH);
    DEF(GLUT_VIDEO_RESIZE_HEIGHT);
#endif

    /* glutUseLayer parameters. */
    DEF(GLUT_NORMAL);
    DEF(GLUT_OVERLAY);

    /* glutGetModifiers return mask. */
    DEF(GLUT_ACTIVE_SHIFT);
    DEF(GLUT_ACTIVE_CTRL);
    DEF(GLUT_ACTIVE_ALT);

    /* glutSetCursor parameters. */
    /* Basic arrows. */
    DEF(GLUT_CURSOR_RIGHT_ARROW);
    DEF(GLUT_CURSOR_LEFT_ARROW);
    /* Symbolic cursor shapes. */
    DEF(GLUT_CURSOR_INFO);
    DEF(GLUT_CURSOR_DESTROY);
    DEF(GLUT_CURSOR_HELP);
    DEF(GLUT_CURSOR_CYCLE);
    DEF(GLUT_CURSOR_SPRAY);
    DEF(GLUT_CURSOR_WAIT);
    DEF(GLUT_CURSOR_TEXT);
    DEF(GLUT_CURSOR_CROSSHAIR);
    /* Directional cursors. */
    DEF(GLUT_CURSOR_UP_DOWN);
    DEF(GLUT_CURSOR_LEFT_RIGHT);
    /* Sizing cursors. */
    DEF(GLUT_CURSOR_TOP_SIDE);
    DEF(GLUT_CURSOR_BOTTOM_SIDE);
    DEF(GLUT_CURSOR_LEFT_SIDE);
    DEF(GLUT_CURSOR_RIGHT_SIDE);
    DEF(GLUT_CURSOR_TOP_LEFT_CORNER);
    DEF(GLUT_CURSOR_TOP_RIGHT_CORNER);
    DEF(GLUT_CURSOR_BOTTOM_RIGHT_CORNER);
    DEF(GLUT_CURSOR_BOTTOM_LEFT_CORNER);
    /* Inherit from parent window. */
    DEF(GLUT_CURSOR_INHERIT);
    /* Blank cursor. */
    DEF(GLUT_CURSOR_NONE);
    /* Fullscreen crosshair (if available). */
    DEF(GLUT_CURSOR_FULL_CROSSHAIR);
#endif
}
