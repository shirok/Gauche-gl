/*
 * gauche-gl.h - Gauche GL extension
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
 *  $Id: gauche-gl.h,v 1.4 2001-10-14 10:01:11 shirok Exp $
 */

#ifndef GAUCHE_GL_H
#define GAUCHE_GL_H

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>

#include <gauche/uvector.h>

extern int Scm_GLGetDoubles(ScmObj val1, ScmObj list,
                            double *result, int maxresult, int minresult);
extern int Scm_GLStateInfoSize(GLenum state);
extern int Scm_GLPixelDataSize(GLsizei w, GLsizei h,
                               GLenum format, GLenum type,
                               int *eltsize, int *packed);

#endif /* GAUCHE_GL_H */
