/*
 * gauche-gl.c - Gauche GL binding
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
 *  $Id: gauche-gl.c,v 1.12 2002-07-19 02:33:27 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
#include "gauche-gl.h"

/* Utility functions */

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
    case GL_UNSIGNED_BYTE_3_3_2:;
    case GL_UNSIGNED_BYTE_2_3_3_REV:;
        if (packed) *packed = TRUE;
        /* FALLTHROUGH */
    case GL_UNSIGNED_BYTE:;
        return SCM_GL_UBYTE;
    case GL_SHORT:;
        return SCM_GL_SHORT;
    case GL_UNSIGNED_SHORT_5_6_5:;
    case GL_UNSIGNED_SHORT_5_6_5_REV:;
    case GL_UNSIGNED_SHORT_4_4_4_4:;
    case GL_UNSIGNED_SHORT_4_4_4_4_REV:;
    case GL_UNSIGNED_SHORT_5_5_5_1:;
    case GL_UNSIGNED_SHORT_1_5_5_5_REV:;
        if (packed) *packed = TRUE;
        /* FALLTHROUGH */
    case GL_UNSIGNED_SHORT:;
        return SCM_GL_USHORT;
    case GL_INT:;
        return SCM_GL_INT;
    case GL_UNSIGNED_INT_8_8_8_8:;
    case GL_UNSIGNED_INT_8_8_8_8_REV:;
    case GL_UNSIGNED_INT_10_10_10_2:;
    case GL_UNSIGNED_INT_2_10_10_10_REV:;
        if (packed) *packed = TRUE;
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
                        int *elttype, int *packed)
{
    int components = 0, packedsize = 0;
    *elttype = Scm_GLPixelDataType(type, packed);

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
    if (*packed) {
        return w*h;
    } else {
        return w*h*components;
    }
}

/* GLU objects */

/* Quadric */
static void quadric_finalize(GC_PTR obj, GC_PTR data)
{
    gluDeleteQuadric((GLUquadricObj*)obj);
}

static ScmObj quadric_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmGluQuadric *q = SCM_NEW(ScmGluQuadric);
    GC_finalization_proc ofn; GC_PTR ocd;
    SCM_SET_CLASS(q, SCM_CLASS_GLU_QUADRIC);
    if ((q->quadric = gluNewQuadric()) == NULL) {
        Scm_Error("gluNewQuadric failed");
    }
    GC_REGISTER_FINALIZER(q, quadric_finalize, NULL, &ofn, &ocd);
    return SCM_OBJ(q);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GluQuadricClass,
                         NULL, NULL, NULL,
                         quadric_allocate,
                         SCM_CLASS_DEFAULT_CPL);

/* Nurbs */
static void nurbs_finalize(GC_PTR obj, GC_PTR data)
{
    gluDeleteNurbsRenderer((GLUnurbsObj*)obj);
}

static ScmObj nurbs_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmGluNurbs *n = SCM_NEW(ScmGluNurbs);
    GC_finalization_proc ofn; GC_PTR ocd;
    SCM_SET_CLASS(n, SCM_CLASS_GLU_NURBS);
    if ((n->nurbs = gluNewNurbsRenderer()) == NULL) {
        Scm_Error("gluNewNurbsRenderer failed");
    }
    GC_REGISTER_FINALIZER(n, nurbs_finalize, NULL, &ofn, &ocd);
    return SCM_OBJ(n);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GluNurbsClass,
                         NULL, NULL, NULL,
                         nurbs_allocate,
                         SCM_CLASS_DEFAULT_CPL);


/* Tesselator */
static void tesselator_finalize(GC_PTR obj, GC_PTR data)
{
    gluDeleteTess((GLUtriangulatorObj*)obj);
}

static ScmObj tesselator_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmGluTesselator *q = SCM_NEW(ScmGluTesselator);
    GC_finalization_proc ofn; GC_PTR ocd;
    SCM_SET_CLASS(q, SCM_CLASS_GLU_TESSELATOR);
    if ((q->tesselator = gluNewTess()) == NULL) {
        Scm_Error("gluNewTess failed");
    }
    GC_REGISTER_FINALIZER(q, tesselator_finalize, NULL, &ofn, &ocd);
    return SCM_OBJ(q);
}

SCM_DEFINE_BUILTIN_CLASS(Scm_GluTesselatorClass,
                         NULL, NULL, NULL,
                         tesselator_allocate,
                         SCM_CLASS_DEFAULT_CPL);


/* Initialization */
extern void Scm_Init_gl_lib(ScmModule *mod);
extern void Scm_Init_gl_syms(ScmModule *mod);
extern void Scm_Init_glu_lib(ScmModule *mod);

void Scm_Init_gauche_gl(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(gauche_gl);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl", TRUE));
    Scm_InitBuiltinClass(&Scm_GluQuadricClass, "glu-quadric",
                         NULL, sizeof(Scm_GluQuadricClass)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_GluNurbsClass, "glu-nurbs",
                         NULL, sizeof(Scm_GluNurbsClass)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_GluTesselatorClass, "glu-tesselator",
                         NULL, sizeof(Scm_GluTesselatorClass)/sizeof(ScmObj),
                         mod);
    Scm_Init_gl_lib(mod);
    Scm_Init_gl_syms(mod);
    Scm_Init_glu_lib(mod);

    /* Constants that may not defined in some platforms */
#define DEF(name)  SCM_DEFINE(mod, #name, Scm_MakeInteger(name))

#ifdef GL_TEXTURE_BINDING_3D
    DEF(GL_TEXTURE_BINDING_3D);
#endif
#ifdef GL_TEXTURE_3D_BINDING_EXT
    DEF(GL_TEXTURE_3D_BINDING_EXT);
#endif
#ifdef GL_COLOR_TABLE_FORMAT
    DEF(GL_COLOR_TABLE_FORMAT);
#endif
#ifdef GL_COLOR_TABLE_FORMAT_EXT
    DEF(GL_COLOR_TABLE_FORMAT_EXT);
#endif
#ifdef GL_COLOR_TABLE_FORMAT_SGI
    DEF(GL_COLOR_TABLE_FORMAT_SGI);
#endif
#ifdef GL_COLOR_TABLE_WIDTH
    DEF(GL_COLOR_TABLE_WIDTH);
#endif
#ifdef GL_COLOR_TABLE_WIDTH_EXT
    DEF(GL_COLOR_TABLE_WIDTH_EXT);
#endif
#ifdef GL_COLOR_TABLE_WIDTH_SGI
    DEF(GL_COLOR_TABLE_WIDTH_SGI);
#endif
#ifdef GL_COLOR_TABLE_RED_SIZE
    DEF(GL_COLOR_TABLE_RED_SIZE);
#endif
#ifdef GL_COLOR_TABLE_RED_SIZE_EXT
    DEF(GL_COLOR_TABLE_RED_SIZE_EXT);
#endif
#ifdef GL_COLOR_TABLE_RED_SIZE_SGI
    DEF(GL_COLOR_TABLE_RED_SIZE_SGI);
#endif
#ifdef GL_COLOR_TABLE_GREEN_SIZE
    DEF(GL_COLOR_TABLE_GREEN_SIZE);
#endif
#ifdef GL_COLOR_TABLE_GREEN_SIZE_EXT
    DEF(GL_COLOR_TABLE_GREEN_SIZE_EXT);
#endif
#ifdef GL_COLOR_TABLE_GREEN_SIZE_SGI
    DEF(GL_COLOR_TABLE_GREEN_SIZE_SGI);
#endif
#ifdef GL_COLOR_TABLE_BLUE_SIZE
    DEF(GL_COLOR_TABLE_BLUE_SIZE);
#endif
#ifdef GL_COLOR_TABLE_BLUE_SIZE_EXT
    DEF(GL_COLOR_TABLE_BLUE_SIZE_EXT);
#endif
#ifdef GL_COLOR_TABLE_BLUE_SIZE_SGI
    DEF(GL_COLOR_TABLE_BLUE_SIZE_SGI);
#endif
#ifdef GL_COLOR_TABLE_ALPHA_SIZE
    DEF(GL_COLOR_TABLE_ALPHA_SIZE);
#endif
#ifdef GL_COLOR_TABLE_ALPHA_SIZE_EXT
    DEF(GL_COLOR_TABLE_ALPHA_SIZE_EXT);
#endif
#ifdef GL_COLOR_TABLE_ALPHA_SIZE_SGI
    DEF(GL_COLOR_TABLE_ALPHA_SIZE_SGI);
#endif
#ifdef GL_COLOR_TABLE_LUMINANCE_SIZE
    DEF(GL_COLOR_TABLE_LUMINANCE_SIZE);
#endif
#ifdef GL_COLOR_TABLE_LUMINANCE_SIZE_EXT
    DEF(GL_COLOR_TABLE_LUMINANCE_SIZE_EXT);
#endif
#ifdef GL_COLOR_TABLE_LUMINANCE_SIZE_SGI
    DEF(GL_COLOR_TABLE_LUMINANCE_SIZE_SGI);
#endif
#ifdef GL_COLOR_TABLE_INTENSITY_SIZE
    DEF(GL_COLOR_TABLE_INTENSITY_SIZE);
#endif
#ifdef GL_COLOR_TABLE_INTENSITY_SIZE_EXT
    DEF(GL_COLOR_TABLE_INTENSITY_SIZE_EXT);
#endif
#ifdef GL_COLOR_TABLE_INTENSITY_SIZE_SGI
    DEF(GL_COLOR_TABLE_INTENSITY_SIZE_SGI);
#endif

#ifdef GL_TEXTURE_INDEX_SIZE_EXT
    DEF(GL_TEXTURE_INDEX_SIZE_EXT);
#endif

#ifdef GL_COLOR_INDEX1_EXT
    DEF(GL_COLOR_INDEX1_EXT);
#endif
#ifdef GL_COLOR_INDEX2_EXT
    DEF(GL_COLOR_INDEX2_EXT);
#endif
#ifdef GL_COLOR_INDEX4_EXT
    DEF(GL_COLOR_INDEX4_EXT);
#endif
#ifdef GL_COLOR_INDEX8_EXT
    DEF(GL_COLOR_INDEX8_EXT);
#endif
#ifdef GL_COLOR_INDEX12_EXT
    DEF(GL_COLOR_INDEX12_EXT);
#endif
#ifdef GL_COLOR_INDEX16_EXT
    DEF(GL_COLOR_INDEX16_EXT);
#endif

#ifdef GL_SHARED_TEXTURE_PALETTE_EXT
    DEF(GL_SHARED_TEXTURE_PALETTE_EXT);
#endif

#ifdef GL_POINT_SIZE_MIN_EXT
    DEF(GL_POINT_SIZE_MIN_EXT);
#endif
#ifdef GL_POINT_SIZE_MIN_SGI
    DEF(GL_POINT_SIZE_MIN_SGI);
#endif
#ifdef GL_POINT_SIZE_MAX_EXT
    DEF(GL_POINT_SIZE_MAX_EXT);
#endif
#ifdef GL_POINT_SIZE_MAX_SGI
    DEF(GL_POINT_SIZE_MAX_SGI);
#endif
#ifdef GL_POINT_FADE_THRESHOLD_SIZE_EXT
    DEF(GL_POINT_FADE_THRESHOLD_SIZE_EXT);
#endif
#ifdef GL_POINT_FADE_THRESHOLD_SIZE_SGI
    DEF(GL_POINT_FADE_THRESHOLD_SIZE_SGI);
#endif
#ifdef GL_DISTANCE_ATTENUATION_EXT
    DEF(GL_DISTANCE_ATTENUATION_EXT);
#endif
#ifdef GL_DISTANCE_ATTENUATION_SGI
    DEF(GL_DISTANCE_ATTENUATION_SGI);
#endif

#ifdef GL_SELECTED_TEXTURE_SGIS
    DEF(GL_SELECTED_TEXTURE_SGIS);
#endif
#ifdef GL_SELECTED_TEXTURE_COORD_SET_SGIS
    DEF(GL_SELECTED_TEXTURE_COORD_SET_SGIS);
#endif
#ifdef GL_SELECTED_TEXTURE_EXT
    DEF(GL_SELECTED_TEXTURE_EXT);
#endif
#ifdef GL_SELECTED_TEXTURE_COORD_SET_EXT
    DEF(GL_SELECTED_TEXTURE_COORD_SET_EXT);
#endif
#ifdef GL_SELECTED_TEXTURE_TRANSFORM_EXT
    DEF(GL_SELECTED_TEXTURE_TRANSFORM_EXT);
#endif

#ifdef GL_TEXTURE0_SGIS
    DEF(GL_TEXTURE0_SGIS);
#endif
#ifdef GL_TEXTURE0_EXT
    DEF(GL_TEXTURE0_EXT);
#endif
#ifdef GL_TEXTURE1_SGIS
    DEF(GL_TEXTURE1_SGIS);
#endif
#ifdef GL_TEXTURE1_EXT
    DEF(GL_TEXTURE1_EXT);
#endif
#ifdef GL_TEXTUER2_SGIS
    DEF(GL_TEXTUER2_SGIS);
#endif
#ifdef GL_TEXTURE2_EXT
    DEF(GL_TEXTURE2_EXT);
#endif
#ifdef GL_TEXTURE3_SGIS
    DEF(GL_TEXTURE3_SGIS);
#endif
#ifdef GL_TEXTURE3_EXT
    DEF(GL_TEXTURE3_EXT);
#endif
#ifdef GL_TEXTURE_COORD_SET_SOURCE_SGIS
    DEF(GL_TEXTURE_COORD_SET_SOURCE_SGIS);
#endif
#ifdef GL_TEXTURE_COORD_SET_SOURCE_EXT
    DEF(GL_TEXTURE_COORD_SET_SOURCE_EXT);
#endif

#ifdef GL_MAX_TEXTURES_SGIS
    DEF(GL_MAX_TEXTURES_SGIS);
#endif
#ifdef GL_MAX_TEXTURES_EXT
    DEF(GL_MAX_TEXTURES_EXT);
#endif
#ifdef GL_MAX_TEXTURE_COORD_SETS_EXT
    DEF(GL_MAX_TEXTURE_COORD_SETS_EXT);
#endif
#ifdef GL_TEXTURE_ENV_COORD_SET_EXT
    DEF(GL_TEXTURE_ENV_COORD_SET_EXT);
#endif
}
