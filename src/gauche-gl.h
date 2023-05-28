/*
 * gauche-gl.h - Gauche GL extension
 *
 *   Copyright (c) 2001-2023  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_GL_H
#define GAUCHE_GL_H

#include "gauche/gl-config.h"

#if MacOSX
#define GL_SILENCE_DEPRECATION  /* OSX deprecated GL */
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#include <GLUT/glut.h> /* for glutGetProcAddress. */
#elif HAVE_GL_GLEW_H
#include <GL/glew.h>
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
