/*
 * gauche-math3d.c - 3D vector and matrix arithmetics
 *
 *  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: gauche-math3d.c,v 1.1 2002-09-27 05:09:26 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
#include "gauche/math3d.h"

#if 0 /*not yet working*/
/*
 * Sequence CPL
 */
static ScmClass *sequenceCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_SequenceClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

#define MAKE_F32V(len)  SCM_F32VECTOR(Scm_MakeF32Vector(len, 0.0))

#define CHECK_F32V(v, len)                                      \
    do {                                                        \
      if (SCM_F32VECTOR_SIZE(v) != (len)) {                     \
        Scm_Error("f32vector of size %d required, but got %S",  \
                  (len), (v));                                  \
      }                                                         \
    } while (0)
    
/*=============================================================
 * Vec4
 */
static void vec4_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(vec4 %gf %g %g %g)",
               SCM_VEC4_D(obj)[0],
               SCM_VEC4_D(obj)[1],
               SCM_VEC4_D(obj)[2],
               SCM_VEC4_D(obj)[3]);
}

static int vec4_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        if (SCM_VEC4_D(x)[0] == SCM_VEC4_D(y)[0]
            && SCM_VEC4_D(x)[1] == SCM_VEC4_D(y)[1]
            && SCM_VEC4_D(x)[2] == SCM_VEC4_D(y)[2]
            && SCM_VEC4_D(x)[3] == SCM_VEC4_D(y)[3]) {
            return 0;
        } else {
            return -1;
        }
    } else {
        Scm_Error("can't order %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(ScmVec4Class, vec4_print, vec4_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakeVec4(float x, float y, float z, float w)
{
    ScmVec4 *v = SCM_NEW(ScmVec4);
    SCM_SET_CLASS(v, SCM_CLASS_VEC4);
    v->v = MAKE_F32V(4);
    SCM_VEC4_D(v)[0] = x; SCM_VEC4_D(v)[1] = y;
    SCM_VEC4_D(v)[2] = z; SCM_VEC4_D(v)[3] = w;
    return SCM_OBJ(v);
}

ScmObj Scm_MakeVec4V(ScmF32Vector *fv)
{
    ScmVec4 *v;
    CHECK_F32V(fv, 4);
    v = SCM_NEW(ScmVec4);
    SCM_SET_CLASS(v, SCM_CLASS_VEC4);
    v->v = fv;
    return SCM_OBJ(v);
}

ScmObj Scm_MakeVec4v(const float *d)
{
    if (d) return Scm_MakeVec4(d[0], d[1], d[2], d[3]);
    else   return Scm_MakeVec4(0.0, 0.0, 0.0, 0.0);
}

/* common routine for list->3dvector and list->3dpoint */

static void list2vec(ScmObj l, float *d, float init3)
{
    int i;
    ScmObj lp = l;
    for (i=0; i<3; i++, lp = SCM_CDR(lp)) {
        if (!SCM_PAIRP(lp)) goto badlist;
        if (!SCM_REALP(SCM_CAR(lp))) goto badlist;
        d[i] = (float)Scm_GetDouble(SCM_CAR(lp));
    }
    if (SCM_PAIRP(lp)) {
        if (!SCM_REALP(SCM_CAR(lp))) goto badlist;
        d[3] = (float)Scm_GetDouble(SCM_CAR(lp));
        lp = SCM_CDR(lp);
    } else {
        d[3] = 0.0;
    }
    if (SCM_NULLP(lp)) return;
  badlist:
    Scm_Error("list of 3 or 4 real numbers required, but got %S", l);
}

ScmObj Scm_ListToVector(ScmObj l)
{
    float d[4];
    list2vec(l, d, 0.0);
    return Scm_MakeVec4v(d);
}

ScmObj ScmVec4ToList(const ScmVec4 *v)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_VEC4_D(v)[0]),
                     Scm_MakeFlonum(SCM_VEC4_D(v)[1]),
                     Scm_MakeFlonum(SCM_VEC4_D(v)[2]),
                     Scm_MakeFlonum(SCM_VEC4_D(v)[3]));
}

float ScmVec4Dot(const ScmVec4 *p, const ScmVec4 *q)
{
    return (SCM_VEC4_DOTV(SCM_VEC4_D(p), SCM_VEC4_D(q)));
}

float ScmVec4Dotv(const float *p, const float *q)
{
    return SCM_VEC4_DOTV(p, q);
}

ScmObj ScmVec4Cross(const ScmVec4 *p, const ScmVec4 *q)
{
    float r[4];
    SCM_VEC4_CROSSV(r, SCM_VEC4_D(p), SCM_VEC4_D(q));
    return Scm_MakeVec4v(r);
}

void ScmVec4Crossv(float *r, const float *p, const float *q)
{
    SCM_VEC4_CROSSV(r, p, q);
}

ScmObj ScmVec4Normalize(const ScmVec4 *p)
{
    float r[4];
    r[0] = SCM_VEC4_D(p)[0];
    r[1] = SCM_VEC4_D(p)[1];
    r[2] = SCM_VEC4_D(p)[2];
    r[3] = SCM_VEC4_D(p)[3];
    SCM_VEC4_NORMALIZEV(r);
    return Scm_MakeVec4v(r);
}

void ScmVec4Normalizev(float *p)
{
    SCM_VEC4_NORMALIZEV(p);
}

ScmObj ScmVec4NormalizeX(ScmVec4 *p)
{
    SCM_VEC4_NORMALIZEV(SCM_VEC4_D(p));
    return SCM_OBJ(p);
}

ScmObj ScmVec4Add(const ScmVec4 *p, const ScmVec4 *q)
{
    float r[4];
    SCM_VEC4_ADDV(r, SCM_VEC4_D(p), SCM_VEC4_D(q));
    return Scm_MakeVec4v(r);
}

void ScmVec4Addv(float *r, const float *p, const float *q)
{
    SCM_VEC4_ADDV(r, p, q);
}

ScmObj ScmVec4Sub(const ScmVec4 *p, const ScmVec4 *q)
{
    float r[4];
    SCM_VEC4_SUBV(r, SCM_VEC4_D(p), SCM_VEC4_D(q));
    return Scm_MakeVec4v(r);
}

void ScmVec4Subv(float *r, const float *p, const float *q)
{
    SCM_VEC4_SUBV(r, p, q);
}

/*=============================================================
 * VectorArray
 */

static void vec4_array_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    ScmVec4Array *va = SCM_VEC4_ARRAY(obj);
    int len = SCM_VEC4_ARRAY_LENGTH(va), i;
    Scm_Printf(out, "#,(vec4-array %d ", len);
    for (i = 0; i < len; i++) {
        float *z = Scm_Vec4ArrayRefv(va, i);
        Scm_Printf(out, "(%g %g %g %g) ", z[0], z[1], z[2], z[3]);
    }
    Scm_Printf(out, ")");
}

static int vec4_array_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        int i, len = SCM_VEC4_ARRAY_LENGTH(x);
        float *p = SCM_VEC4_ARRAY_D(x), *q = SCM_VEC4_ARRAY_D(y);
        if (len != SCM_VEC4_ARRAY_LENGTH(y)) return 0;
        for (i=0; i<len*4; i++) {
            if (*p++ != *q++) return 0;
        }
        return -1;
    } else {
        Scm_Error("can't order %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(ScmVec4ArrayClass, vec4_array_print, vec4_array_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakeVec4Arrayv(int nvecs, const float *init)
{
    int i, j;
    ScmVec4Array *a;
    SCM_ASSERT(nvecs >= 0);
    a = SCM_NEW(ScmVec4Array);
    SCM_SET_CLASS(a, SCM_CLASS_VEC4_ARRAY);
    a->length = nvecs;
    a->v = MAKE_F32V(nvecs*4);
    if (init) {
        for (i=0;i<nvecs;i++) {
            SCM_VEC4_ARRAY_SET(a, i, init[0], init[1], init[2], init[3]);
        }
    } else {
        for (i=0;i<nvecs*4;i++) SCM_VEC4_ARRAY_SET(a)[i] = 0.0;
    }
    return SCM_OBJ(a);
}

ScmObj Scm_MakeVec4ArrayV(Scm_F32Vector *fv)
{
    int size;
    ScmVec4Array *a;
    size = SCM_F32VECTOR_SIZE(fv);
    if (size % 4 != 0) {
        Scm_Error("f32vector size must be multiple of 4, but got %S", fv);
    }
    a = SCM_NEW(ScmVec4Array);
    SCM_SET_CLASS(a, SCM_CLASS_VEC4_ARRAY);
    a->length = size/4;
    a->v = fv;
    return SCM_OBJ(a);
}

ScmObj ScmVec4ArrayRef(const ScmVec4Array *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_VEC4_ARRAY_LENGTH(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_MakeVec4v(SCM_VEC4_ARRAY_REFV(a, n));
}

void ScmVec4ArraySet(ScmVec4Array *a, int n, ScmVec4 *v)
{
    ScmVec4ArraySetv(a, n, SCM_VEC4_D(v));
}

void ScmVec4ArraySetv(ScmVec4Array *a, int n, float d[]
{
    if (n < 0 || n >= SCM_VEC4_ARRAY_LENGTH(a)) {
        Scm_Error("index out of range");
    }
    SCM_VEC4_ARRAY_SET(a, n, d[0], d[1], d[2], d[3]);
}

/*=============================================================
 * Pointf - is just a vector with different default value.
 */
static void point4_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(point4 %g %g %g %g)",
               SCM_VEC4_D(obj)[0],
               SCM_VEC4_D(obj)[1],
               SCM_VEC4_D(obj)[2],
               SCM_VEC4_D(obj)[3]);
}

#define point4_compare vec4_compare

SCM_DEFINE_BUILTIN_CLASS(Scm_Point4Class, point4_print, point4_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakePoint4(float x, float y, float z, float w)
{
    BnPoint *v = SCM_NEW(BnPoint);
    SCM_SET_CLASS(v, SCM_CLASS_POINT4);
    v->v = MAKE_F32V(4);
    SCM_POINT4_D(v)[0] = x; SCM_POINT4_D(v)[1] = y;
    SCM_POINT4_D(v)[2] = z; SCM_POINT4_D(v)[3] = w;
    return SCM_OBJ(v);
}

ScmObj Scm_MakePoint4v(const float *d)
{
    if (d) return Scm_MakePoint4(d[0], d[1], d[2], d[3]);
    else   return Scm_MakePoint4(0.0, 0.0, 0.0, 0.0);
}

ScmObj Scm_MakePoint4V(ScmF32Vector *fv)
{
    ScmPoint4 *v;
    CHECK_F32V(fv, 4);
    v = SCM_NEW(ScmPoint4);
    SCM_SET_CLASS(v, SCM_CLASS_POINT4);
    v->v = fv;
    return SCM_OBJ(v);
}

ScmObj Scm_ListToPoint4(ScmObj l)
{
    float d[4];
    list2vec(l, d, 1.0);
    return Scm_MakePoint4v(d);
}

ScmObj Scm_Point4ToList(const ScmPoint4 *p)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_POINT4_D(p)[0]),
                     Scm_MakeFlonum(SCM_POINT4_D(p)[1]),
                     Scm_MakeFlonum(SCM_POINT4_D(p)[2]),
                     Scm_MakeFlonum(SCM_POINT4_D(p)[3]));
}

ScmObj Scm_Point4Add(const ScmPoint4 *p, const ScmVec4 *q)
{
    float r[4];
    SCM_VEC4_ADDV(r, SCM_POINT4_D(p), SCM_VEC4_D(q));
    return Scm_MakePoint4v(r);
}

ScmObj Scm_Point4Sub(const ScmVec4 *p, const ScmObj q)
{
    float r[4];
    if (SCM_POINTP(q)) {
        SCM_VEC4_SUBV(r, SCM_POINT4_D(p), SCM_POINT4_D(q));
        return Scm_MakeVec4v(r);
    }
    if (SCM_VEC4P(q)) {
        SCM_VEC4_SUBV(r, SCM_POINT4_D(p), SCM_VEC4_D(q));
        return Scm_MakePoint4(r);
    }
    Scm_Error("<point4> or <vec4> required, but got %S", q);
    return SCM_UNDEFINED;
}

/*=============================================================
 * Point4Array
 */

static void point4_array_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    ScmPoint4Array *va = SCM_POINT4_ARRAY(obj);
    int len = SCM_POINT4_ARRAY_LENGTH(va);
    Scm_Printf(out, "#,(point4-array %d ", len);
    for (i = 0; i < len; i++) {
        float *z = Scm_Point4ArrayRefv(va, i);
        Scm_Printf(out, "(%g %g %g %g) ", z[0], z[1], z[2], z[3]);
    }
    Scm_Printf(out, ")");
}

#define point4_array_compare vec4_array_compare

SCM_DEFINE_BUILTIN_CLASS(Scm_Point4ArrayClass, point4_array_print,
                         point4_array_compare, NULL, NULL, sequenceCPL);

ScmObj Scm_MakePoint4Arrayv(int len, const float *init)
{
    int i, j;
    ScmPoint4Array *a;
    SCM_ASSERT(len >= 0);
    a = SCM_NEW(ScmPoint4Array);
    SCM_SET_CLASS(a, SCM_CLASS_POINT_ARRAY);
    a->length = len;
    a->v = MAKE_F32V(len*4);
    if (init) {
        for (i=0;i<len;i++) {
            SCM_POINT4_ARRAY_SET(a, i, init[0], init[1], init[2], init[3]);
        }
    } else {
        for (i=0;i<len*4;i++) a->d[i] = 0.0;
    }
    return SCM_OBJ(a);
}

ScmObj Scm_MakePoint4ArrayV(ScmF32Vector *fv)
{
    int size;
    ScmPoint4Array *a;
    size = SCM_F32VECTOR_SIZE(fv);
    if (size % 4 != 0) {
        Scm_Error("f32vector size must be multiple of 4, but got %S", fv);
    }
    a = SCM_NEW(ScmPoint4Array);
    SCM_SET_CLASS(a, SCM_CLASS_POINT4_ARRAY);
    a->length = size/4;
    a->v = fv;
    return SCM_OBJ(a);
}

ScmObj Scm_Point4ArrayRef(const ScmPoint4Array *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_POINT4_ARRAY_LENGTH(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_MakePoint4(SCM_POINT4_ARRAY_REFV(a, n));
}

float *Scm_Point4ArrayRefv(ScmPoint4Array *a, int n)
{
    return SCM_POINT4_ARRAY_REFV(a, n);
}

void Scm_Point4ArraySet(ScmPoint4Array *a, int n, ScmPoint4 *v)
{
    if (n < 0 || n >= SCM_POINT_ARRAY_LENGTH(a)) {
        Scm_Error("index out of range");
    }
    SCM_POINT4_ARRAY_SET(a, n,
                         SCM_POINT4_D(v)[0],
                         SCM_POINT4_D(v)[1],
                         SCM_POINT4_D(v)[2],
                         SCM_POINT4_D(v)[3]);
}

void Scm_Point4ArraySetv(ScmPointArray *a, int n, float d[])
{
    SCM_POINT4_ARRAY_SET(a, n, d[0], d[1], d[2], d[3]);
}

/*
 * Initialization
 */
extern void Scm_Init_math3d_lib(ScmModule *mod);

void Scm_Init_gauche_math3d(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(gauche_math3d);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl.math3d", TRUE));
    Scm_Init_math3d_lib(mod);
}
#endif /* not yet working */
