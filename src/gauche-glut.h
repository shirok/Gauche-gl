/*
 * gauche-glut.h - Gauche GLUT binding
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
 *  $Id: gauche-glut.h,v 1.1 2001-09-30 03:58:33 shirok Exp $
 */

#ifndef GAUCHE_GLUT_H
#define GAUCHE_GLUT_H

/* opaque pointer for glut font */
typedef struct ScmGlutFontRec {
    SCM_HEADER;
    void *font;
} ScmGlutFont;

extern ScmClass Scm_GlutFontClass;
#define SCM_CLASS_GLUT_FONT   (&Scm_GlutFontClass)
#define SCM_GLUT_FONT_P(obj)  (SCM_XTYPEP(obj, SCM_CLASS_GLUT_FONT))
#define SCM_GLUT_FONT(obj)    ((ScmGlutFont*)(obj))

#endif /*GAUCHE_GLUT_H */

