/*
 * gauche-gl.h - Gauche GL extension
 *
 *  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: gauche-gl.h,v 1.14 2005-06-02 00:56:00 shirok Exp $
 */

#ifndef GAUCHE_GL_H
#define GAUCHE_GL_H

#define GL_GLEXT_PROTOTYPES

#if MacOSX
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <OpenGL/glext.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#endif

#ifdef HAVE_GL_GLX_H
#include <GL/glx.h>
#endif

#ifdef HAVE_GL_GLEXT_H
#include <GL/glext.h>
#endif
#include <gauche/uvector.h>
#include "gauche/math3d.h"

/* GL auxiliary routines */

/* get extension function pointer */
void *Scm_GLGetProcAddress(const char *name);

/* acceptable data type */
enum {
    SCM_GL_BYTE,
    SCM_GL_UBYTE,
    SCM_GL_SHORT,
    SCM_GL_USHORT,
    SCM_GL_INT,
    SCM_GL_UINT,
    SCM_GL_FLOAT,
    SCM_GL_FLOAT_OR_INT,
    SCM_GL_DOUBLE
};

extern int Scm_GLGetDoubles(ScmObj val1, ScmObj list,
                            double *result, int maxresult, int minresult);
extern int Scm_GLStateInfoSize(GLenum state);

extern int Scm_GLPixelDataType(GLenum type, int *packed);
extern int Scm_GLPixelDataSize(GLsizei w, GLsizei h,
                               GLenum format, GLenum type,
                               int *elttype, int *packed);
extern void *Scm_GLPixelDataCheck(ScmObj pixels, int elttype, int size);

/* GLU objects */

/* Quadrics */
typedef struct ScmGluQuadricRec {
    SCM_HEADER;
    GLUquadricObj* quadric;
} ScmGluQuadric;

SCM_CLASS_DECL(Scm_GluQuadricClass);
#define SCM_CLASS_GLU_QUADRIC    (&Scm_GluQuadricClass)
#define SCM_GLU_QUADRIC(obj)     ((ScmGluQuadric*)(obj))
#define SCM_GLU_QUADRIC_P(obj)   SCM_XTYPEP(obj, SCM_CLASS_GLU_QUADRIC)

/* Nurbs */
typedef struct ScmGluNurbsRec {
    SCM_HEADER;
    GLUnurbsObj* nurbs;
} ScmGluNurbs;

SCM_CLASS_DECL(Scm_GluNurbsClass);
#define SCM_CLASS_GLU_NURBS    (&Scm_GluNurbsClass)
#define SCM_GLU_NURBS(obj)     ((ScmGluNurbs*)(obj))
#define SCM_GLU_NURBS_P(obj)   SCM_XTYPEP(obj, SCM_CLASS_GLU_NURBS)

extern ScmObj Scm_GLUMakeNurbs(GLUnurbsObj*);

/* Tessalator */
/* GLU 1.0 and 1.1 interface */
typedef struct ScmGluTesselatorRec {
    SCM_HEADER;
    GLUtriangulatorObj* tesselator;
} ScmGluTesselator;

SCM_CLASS_DECL(Scm_GluTesselatorClass);
#define SCM_CLASS_GLU_TESSELATOR    (&Scm_GluTesselatorClass)
#define SCM_GLU_TESSELATOR(obj)     ((ScmGluTesselator*)(obj))
#define SCM_GLU_TESSELATOR_P(obj)   SCM_XTYPEP(obj, SCM_CLASS_GLU_TESSELATOR)

extern ScmObj Scm_GLUMakeTesselator(GLUtriangulatorObj*);

/* GLU 1.2 and later uses GLUtesselator. (not yet implemented */


#endif /* GAUCHE_GL_H */
