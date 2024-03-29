/*
 * gauche-gl.c - Gauche GL binding
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

#include <gauche.h>
#include <gauche/class.h>
#include <gauche/extend.h>
#include "gauche-gl.h"
#include "gl-syms.h"

/*
 * GLboolean vector
 */

static void glboolvec_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static int  glboolvec_compare(ScmObj x, ScmObj y, int equalp);

static ScmClass *sequenceCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_SequenceClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_GLBooleanVectorClass, glboolvec_print,
                         glboolvec_compare, NULL, NULL,
                         sequenceCPL);

static ScmGLBooleanVector *glboolvec_allocate(int size,
                                              GLboolean *elts)
{
    ScmGLBooleanVector *v = SCM_NEW(ScmGLBooleanVector);
    SCM_SET_CLASS(v, SCM_CLASS_GL_BOOLEAN_VECTOR);
    if (elts == NULL) {
        elts = SCM_NEW_ATOMIC2(GLboolean*, size*sizeof(GLboolean));
    }
    v->size = size;
    v->elements = elts;
    return v;
}

ScmObj Scm_MakeGLBooleanVector(int size, GLboolean fill)
{
    int i;
    ScmGLBooleanVector *v = glboolvec_allocate(size, NULL);
    for (i=0; i<size; i++) {
        v->elements[i] = fill;
    }
    return SCM_OBJ(v);
}

ScmObj Scm_MakeGLBooleanVectorFromArray(int size, const GLboolean arr[])
{
    int i;
    ScmGLBooleanVector *v = glboolvec_allocate(size, NULL);
    for (i=0; i<size; i++) {
        v->elements[i] = arr[i];
    }
    return SCM_OBJ(v);
}

ScmObj Scm_MakeGLBooleanVectorFromArrayShared(int size, GLboolean arr[])
{
    ScmGLBooleanVector *v = glboolvec_allocate(size, arr);
    return SCM_OBJ(v);
}

ScmObj Scm_ListToGLBooleanVector(ScmObj lis)
{
    int len = Scm_Length(lis), i;
    ScmObj lp;
    ScmGLBooleanVector *v;

    if (len < 0) Scm_Error("proper list required, but got %S", lis);
    v = glboolvec_allocate(len, NULL);
    i = 0;
    SCM_FOR_EACH(lp, lis) {
        v->elements[i++] = SCM_FALSEP(SCM_CAR(lp))? GL_FALSE : GL_TRUE;
    }
    return SCM_OBJ(v);
}

static void glboolvec_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx SCM_UNUSED)
{
    ScmGLBooleanVector *v = SCM_GL_BOOLEAN_VECTOR(obj);
    int i, size = v->size;
    Scm_Printf(port, "#,(gl-boolean-vector");
    for (i=0; i<size; i++) {
        Scm_Printf(port, (v->elements[i]? " #t" : " #f"));
    }
    Scm_Printf(port, ")");
}

static int glboolvec_compare(ScmObj x, ScmObj y, int equalp)
{
    int i, xsize, ysize;
    if (!equalp) {
        Scm_Error("cannot compare <gl-boolean-vector>s: %S, %S", x, y);
    }
    xsize = SCM_GL_BOOLEAN_VECTOR(x)->size;
    ysize = SCM_GL_BOOLEAN_VECTOR(y)->size;
    if (xsize != ysize) return 1;
    for (i=0; i<xsize; i++) {
        if ((!SCM_GL_BOOLEAN_VECTOR(x)->elements[i])
            !=
            (!SCM_GL_BOOLEAN_VECTOR(y)->elements[i])) {
            return 1;
        }
    }
    return 0;
}


/* Utility functions */

/* Get extention procedure address.
   The way to get procedure address differs by platforms:
     wglGetProcAddress()
     glXGetProcAddress() / glXGetProcAddressARB()
   This function is to hide the difference.  The coverage is
   incomplete, though.

   NB: It seems that we'd better to use glXGetProcAddressARB.
   http://lists.freedesktop.org/archives/xorg/2005-November/011279.html
*/
void *Scm_GLGetProcAddress(const char *name)
{
#if defined(GLX_ARB_get_proc_address)
    return glXGetProcAddressARB((const GLubyte*)name);
#elif defined(GLX_VERSION_1_4)
    return glXGetProcAddress(name);
#elif defined(MacOSX)
    return glutGetProcAddress(name);
#elif defined(__CYGWIN__) && defined(X_DISPLAY_MISSING)
    return wglGetProcAddress(name);
#endif /* !defined(GLX_VERSION_1_4) && !defined(GLX_ARB_get_proc_address) */
    Scm_Error("GL extension %s is not supported on this platform", name);
}

/* List of numbers -> array of doubles.  Returns # of elements. */
int Scm_GLGetDoubles(ScmObj val1, ScmObj list, double *result,
                     int maxresult, int minresult)
{
    int i = 0;
    ScmObj lp;

    if (!SCM_UNBOUNDP(val1)) {
        if (!SCM_NUMBERP(val1)) {
            Scm_Error("number required, but got %S", val1);
        }
        result[0] = Scm_GetDouble(val1);
        i++;
    }

    SCM_FOR_EACH(lp, list) {
        if (i >= maxresult) {
            Scm_Error("too many arguments: %S, at most %d allowed",
                      list, maxresult);
        }
        if (!SCM_NUMBERP(SCM_CAR(lp))) {
            Scm_Error("number required, but got %S", SCM_CAR(lp));
        }
        result[i] = Scm_GetDouble(SCM_CAR(lp));
        i++;
    }
    if (i < minresult) {
        Scm_Error("too few argument: %S, at least %d required",
                  list, minresult);
    }
    return i;
}

/* returns # of values returned by glGetTYPEv call for given state.
   -1 if the state can't be queried by glGetTYPEv. */
int Scm_GLStateInfoSize(GLenum state)
{
   switch(state) {
#include "gettype-sizes.c"
   }
   return -1;
}

/* given pixel data type GLenum, returns the appropriate data type. */
int Scm_GLPixelDataType(GLenum type, int *packed)
{
    if (packed) *packed = FALSE; /* default */
    switch (type) {
    case GL_BYTE:;
        return SCM_GL_BYTE;
    case GL_BITMAP:
#if !defined(__CYGWIN__)
    case GL_UNSIGNED_BYTE_3_3_2:;
    case GL_UNSIGNED_BYTE_2_3_3_REV:;
#endif /*!defined(__CYGWIN__)*/
        if (packed) *packed = TRUE;
        /* FALLTHROUGH */
    case GL_UNSIGNED_BYTE:;
        return SCM_GL_UBYTE;
    case GL_SHORT:;
        return SCM_GL_SHORT;
#if !defined(__CYGWIN__)
    case GL_UNSIGNED_SHORT_5_6_5:;
    case GL_UNSIGNED_SHORT_5_6_5_REV:;
    case GL_UNSIGNED_SHORT_4_4_4_4:;
    case GL_UNSIGNED_SHORT_4_4_4_4_REV:;
    case GL_UNSIGNED_SHORT_5_5_5_1:;
    case GL_UNSIGNED_SHORT_1_5_5_5_REV:;
        if (packed) *packed = TRUE;
#endif /*!defined(__CYGWIN__)*/
        /* FALLTHROUGH */
    case GL_UNSIGNED_SHORT:;
        return SCM_GL_USHORT;
    case GL_INT:;
        return SCM_GL_INT;
#if !defined(__CYGWIN__)
    case GL_UNSIGNED_INT_8_8_8_8:;
    case GL_UNSIGNED_INT_8_8_8_8_REV:;
    case GL_UNSIGNED_INT_10_10_10_2:;
    case GL_UNSIGNED_INT_2_10_10_10_REV:;
        if (packed) *packed = TRUE;
#endif /*!defined(__CYGWIN__)*/
        /* FALLTHROUGH */
    case GL_UNSIGNED_INT:;
        return SCM_GL_UINT;
    case GL_FLOAT:;
        return SCM_GL_FLOAT;
    default:
        /* TODO: packedsize types added to GL1.2 */
        Scm_Error("unsupported or invalid pixel data type: %d", type);
    }
    return 0;                   /* NOTREACHED */
}

/* given pixel format and pixel type, return # of elements used for
   pixel data. */
/* TODO: need to take into account the pixel store settings! */
int Scm_GLPixelDataSize(GLsizei w, GLsizei h, GLenum format, GLenum type,
                        int *elttype, int *packedp)
{
    int components = 0, packed = FALSE;
    *elttype = Scm_GLPixelDataType(type, &packed);
    if (packedp) *packedp = packed;

    switch (format) {
    case GL_COLOR_INDEX:;
    case GL_RED:;
    case GL_GREEN:;
    case GL_BLUE:;
    case GL_ALPHA:;
    case GL_LUMINANCE:;
    case GL_STENCIL_INDEX:;
    case GL_DEPTH_COMPONENT:;
        components = 1; break;
    case GL_RGB:;
    /*case GL_BGR:;*/
        components = 3; break;
    case GL_LUMINANCE_ALPHA:;
        components = 2; break;
    case GL_RGBA:;
    /*case GL_BGRA:;*/
        components = 4; break;
    }
    if (type == GL_BITMAP) {
        /* bitmap.  each raster line is rounded up to byte boundary. */
        return ((components*w+7)/8)*h;
    }
    if (packed) {
        return w*h;
    } else {
        return w*h*components;
    }
}

/* Given pixel element type and data size (obtained by Scm_GLPixelDataSize),
   checks the validity of pixel data, and returns a pointer to the data
   area of the pixel data. */
/* TODO: size is not checked yet */
void *Scm_GLPixelDataCheck(ScmObj pixels, int elttype, int size SCM_UNUSED)
{
    switch (elttype) {
    case SCM_GL_BYTE:
        if (!SCM_S8VECTORP(pixels)) {
            Scm_Error("s8vector required, but got %S", pixels);
        }
        return (void*)SCM_S8VECTOR_ELEMENTS(pixels);
    case SCM_GL_UBYTE:
        if (!SCM_U8VECTORP(pixels)) {
            Scm_Error("u8vector required, but got %S", pixels);
        }
        return (void*)SCM_U8VECTOR_ELEMENTS(pixels);
    case SCM_GL_SHORT:
        if (!SCM_S16VECTORP(pixels)) {
            Scm_Error("s16vector required, but got %S", pixels);
        }
        return (void*)SCM_S16VECTOR_ELEMENTS(pixels);
    case SCM_GL_USHORT:
        if (!SCM_U16VECTORP(pixels)) {
            Scm_Error("u16vector required, but got %S", pixels);
        }
        return (void*)SCM_U16VECTOR_ELEMENTS(pixels);
    case SCM_GL_INT:
        if (!SCM_S32VECTORP(pixels)) {
            Scm_Error("s32vector required, but got %S", pixels);
        }
        return (void*)SCM_S32VECTOR_ELEMENTS(pixels);
    case SCM_GL_UINT:
        if (!SCM_U32VECTORP(pixels)) {
            Scm_Error("u32vector required, but got %S", pixels);
        }
        return (void*)SCM_U32VECTOR_ELEMENTS(pixels);
    case SCM_GL_FLOAT:
        if (!SCM_F32VECTORP(pixels)) {
            Scm_Error("f32vector required, but got %S", pixels);
        }
        return (void*)SCM_F32VECTOR_ELEMENTS(pixels);
    case SCM_GL_FLOAT_OR_INT:
        if (SCM_F32VECTORP(pixels)) {
            return (void*)SCM_F32VECTOR_ELEMENTS(pixels);
        } else if (SCM_S32VECTORP(pixels)) {
            return (void*)SCM_S32VECTOR_ELEMENTS(pixels);
        }
        Scm_Error("f32vector or s32vector required, but got %S", pixels);
        return NULL;
    default:
        Scm_Error("Scm_GLPixelDataCheck: unknown element type: %d", elttype);
        return NULL;
    }
}

/* Allocates a uvector of SIZE.  The type of uvector is specified by
   ELTTYPE (SCM_GL_BYTE, SCM_GL_UBYTE, etc.)   Useful for generic handling
   of getting image data.   May return SCM_FALSE if the type is ambiguous. */
ScmObj Scm_GLAllocUVector(int elttype, int size)
{
    switch (elttype) {
    case SCM_GL_BYTE:
        return Scm_MakeS8Vector(size, 0);
    case SCM_GL_UBYTE:
        return Scm_MakeU8Vector(size, 0);
    case SCM_GL_SHORT:
        return Scm_MakeS16Vector(size, 0);
    case SCM_GL_USHORT:
        return Scm_MakeU16Vector(size, 0);
    case SCM_GL_INT:
        return Scm_MakeS32Vector(size, 0);
    case SCM_GL_UINT:
        return Scm_MakeU32Vector(size, 0);
    case SCM_GL_FLOAT:
        return Scm_MakeF32Vector(size, 0);
    case SCM_GL_DOUBLE:
        return Scm_MakeF64Vector(size, 0);
    default:
        return SCM_FALSE;
    }
}

/* GLU objects */

/* Quadric */
static void quadric_finalize(ScmObj obj, void *data SCM_UNUSED)
{
    gluDeleteQuadric(SCM_GLU_QUADRIC(obj)->quadric);
}

static ScmObj quadric_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmGluQuadric *q = SCM_NEW(ScmGluQuadric);
    SCM_SET_CLASS(q, klass);
    if ((q->quadric = gluNewQuadric()) == NULL) {
        Scm_Error("gluNewQuadric failed");
    }
    Scm_RegisterFinalizer(SCM_OBJ(q), quadric_finalize, NULL);
    return SCM_OBJ(q);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GluQuadricClass,
                         NULL, NULL, NULL,
                         quadric_allocate,
                         NULL);

/* Nurbs */
static void nurbs_finalize(ScmObj obj, void *data SCM_UNUSED)
{
    gluDeleteNurbsRenderer(SCM_GLU_NURBS(obj)->nurbs);
}

static ScmObj nurbs_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmGluNurbs *n = SCM_NEW(ScmGluNurbs);
    SCM_SET_CLASS(n, klass);
    if ((n->nurbs = gluNewNurbsRenderer()) == NULL) {
        Scm_Error("gluNewNurbsRenderer failed");
    }
    Scm_RegisterFinalizer(SCM_OBJ(n), nurbs_finalize, NULL);
    return SCM_OBJ(n);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GluNurbsClass,
                         NULL, NULL, NULL,
                         nurbs_allocate,
                         NULL);


/* Tesselator */
static void tesselator_finalize(ScmObj obj, void* data SCM_UNUSED)
{
    gluDeleteTess(SCM_GLU_TESSELATOR(obj)->tesselator);
}

static ScmObj tesselator_allocate(ScmClass *klass, ScmObj initargs SCM_UNUSED)
{
    ScmGluTesselator *q = SCM_NEW(ScmGluTesselator);
    SCM_SET_CLASS(q, klass);
    if ((q->tesselator = gluNewTess()) == NULL) {
        Scm_Error("gluNewTess failed");
    }
    Scm_RegisterFinalizer(SCM_OBJ(q), tesselator_finalize, NULL);
    return SCM_OBJ(q);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GluTesselatorClass,
                         NULL, NULL, NULL,
                         tesselator_allocate,
                         NULL);


/* Initialization */
extern void Scm_Init_gl_lib(ScmModule *mod);
extern void Scm_Init_gl_syms(ScmModule *mod);
extern void Scm_Init_glext_lib(ScmModule *mod);
extern void Scm_Init_glu_lib(ScmModule *mod);

void Scm_Init_libgauche_gl(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(libgauche_gl);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl", TRUE));
    Scm_InitStaticClassWithMeta(&Scm_GLBooleanVectorClass,
                                "<gl-boolean-vector>",
                                mod, NULL, SCM_NIL, NULL, 0);
    Scm_InitStaticClass(&Scm_GluQuadricClass, "<glu-quadric>",
                        mod, NULL, 0);
    Scm_InitStaticClass(&Scm_GluNurbsClass, "<glu-nurbs>",
                        mod, NULL, 0);
    Scm_InitStaticClass(&Scm_GluTesselatorClass, "<glu-tesselator>",
                        mod, NULL, 0);

    Scm_Init_gl_lib(mod);
    Scm_Init_gl_syms(mod);
    Scm_Init_glext_lib(mod);
    Scm_Init_glu_lib(mod);
}
