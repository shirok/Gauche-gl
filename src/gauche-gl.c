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
 *  $Id: gauche-gl.c,v 1.8 2001-10-14 10:01:11 shirok Exp $
 */

#include <gauche.h>
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

/* given pixel format and pixel type, return # of elements used for
   pixel data. */
int Scm_GLPixelDataSize(GLsizei w, GLsizei h, GLenum format, GLenum type,
                        int *eltsize, int *packed)
{
    int components = 0, packedsize = 0, typesize = 0;

    switch (type) {
    case GL_BITMAP: 
        /* special handling is required for bitmap */
        break;
    case GL_UNSIGNED_BYTE:;
    case GL_BYTE:;
        typesize = 1; break;
    case GL_UNSIGNED_BYTE_3_3_2:;
    case GL_UNSIGNED_BYTE_2_3_3_REV:;
        packedsize = 3;
        typesize = 1; break;
    case GL_UNSIGNED_SHORT:;
    case GL_SHORT:;
        typesize = 2; break;
    case GL_UNSIGNED_SHORT_5_6_5:;
    case GL_UNSIGNED_SHORT_5_6_5_REV:;
    case GL_UNSIGNED_SHORT_4_4_4_4:;
    case GL_UNSIGNED_SHORT_4_4_4_4_REV:;
    case GL_UNSIGNED_SHORT_5_5_5_1:;
    case GL_UNSIGNED_SHORT_1_5_5_5_REV:;
        packedsize = 4;
        typesize = 2; break;
    case GL_UNSIGNED_INT:;
    case GL_INT:;
    case GL_FLOAT:;
        typesize = 4; break;
    case GL_UNSIGNED_INT_8_8_8_8:;
    case GL_UNSIGNED_INT_8_8_8_8_REV:;
    case GL_UNSIGNED_INT_10_10_10_2:;
    case GL_UNSIGNED_INT_2_10_10_10_REV:;
        packedsize = 4;
        typesize = 4; break;
    default:
        /* TODO: packedsize types added to GL1.2 */
        Scm_Error("unsupported or invalid pixel data type: %d", type);
    }
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
    if (typesize == 0) {
        /* bitmap.  each raster line is rounded up to byte boundary. */
        *eltsize = 1;
        return ((components*w+7)/8)*h;
    }
    *eltsize = typesize;
    if (packedsize > 0) {
        if  (components != packedsize) {
            Scm_Error("pixel format %d doesn't match pixel data type %d",
                      format, type);
        }
        *packed = TRUE;
        return w*h;
    } else {
        *packed = FALSE;
        return w*h*components;
    }
}

/* Initialization */
extern void Scm_Init_gl_lib(ScmModule *mod);
extern void Scm_Init_glu_lib(ScmModule *mod);

void Scm_Init_gauche_gl(void)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("gl", TRUE));
    Scm_Init_gl_lib(mod);
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
