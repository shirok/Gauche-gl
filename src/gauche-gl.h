/*
 * gauche-gl.h - Gauche GL extension
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
 *  $Id: gauche-gl.h,v 1.20 2008-06-04 11:50:11 shirok Exp $
 */

#ifndef GAUCHE_GL_H
#define GAUCHE_GL_H

#if MacOSX
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h> /* for glutGetProcAddress. */
#else
#include <GL/gl.h>
#include <GL/glu.h>
#endif

#ifdef HAVE_GL_GLX_H
#define GLX_GLXEXT_PROTOTYPES
#include <GL/glx.h>
#endif

#if defined(__CYGWIN__) && defined(X_DISPLAY_MISSING)
#include <windows.h>   /* for wglGetProcAddress */
#endif

#include <gauche/uvector.h>
#include "gauche/math3d.h"

/*
 * GL auxiliary routines
 */

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

extern ScmObj Scm_GLAllocUVector(int type, int size);

/* GLBoolean vector */

typedef struct ScmGLBooleanVectorRec {
    SCM_HEADER;
    int size;
    GLboolean *elements;
} ScmGLBooleanVector;

SCM_CLASS_DECL(Scm_GLBooleanVectorClass);
#define SCM_CLASS_GL_BOOLEAN_VECTOR     (&Scm_GLBooleanVectorClass)
#define SCM_GL_BOOLEAN_VECTOR(obj)      ((ScmGLBooleanVector*)(obj))
#define SCM_GL_BOOLEAN_VECTOR_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_GL_BOOLEAN_VECTOR)
#define SCM_GL_BOOLEAN_VECTOR_SIZE(obj) (SCM_GL_BOOLEAN_VECTOR(obj)->size)
#define SCM_GL_BOOLEAN_VECTOR_ELEMENTS(obj) (SCM_GL_BOOLEAN_VECTOR(obj)->elements)

extern ScmObj Scm_MakeGLBooleanVector(int size, GLboolean fill);
extern ScmObj Scm_MakeGLBooleanVectorFromArray(int size,
                                               const GLboolean arr[]);
extern ScmObj Scm_MakeGLBooleanVectorFromArrayShared(int size,
                                                     GLboolean arr[]);
extern ScmObj Scm_ListToGLBooleanVector(ScmObj lis);

extern ScmObj Scm_GLBooleanVectorToUVector(ScmObj klass,
                                           ScmGLBooleanVector *src);
extern ScmObj Scm_UVectorToGLBooleanVector(ScmObj src);


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
