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
 *  $Id: gauche-glut.c,v 1.4 2002-08-17 11:58:55 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
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
    ScmModule *mod;
    SCM_INIT_EXTENSION(gauche_glut);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl.glut", TRUE));
    Scm_Init_glut_lib(mod);

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
