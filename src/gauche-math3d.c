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
 *  $Id: gauche-math3d.c,v 1.11 2002-09-29 10:57:27 shirok Exp $
 */

#include <gauche.h>
#include <gauche/extend.h>
#include <gauche/uvector.h>
#include "gauche/math3d.h"

/*
 * Sequence CPL
 */
static ScmClass *sequenceCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_SequenceClass),
    SCM_CLASS_STATIC_PTR(Scm_CollectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

#define ALLOC_FV(len)  SCM_NEW_ATOMIC2(float*, (len)*sizeof(float))

#define CHECK_F32V(v, len)                                      \
    do {                                                        \
      if (SCM_F32VECTOR_SIZE(v) != (len)) {                     \
        Scm_Error("f32vector of size %d required, but got %S",  \
                  (len), (v));                                  \
      }                                                         \
    } while (0)
    
/*=============================================================
 * Vector4f
 */
static void vec_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(vector4f %g %g %g %g)",
               SCM_VECTOR4F_D(obj)[0],
               SCM_VECTOR4F_D(obj)[1],
               SCM_VECTOR4F_D(obj)[2],
               SCM_VECTOR4F_D(obj)[3]);
}

static int vec_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        if (SCM_VECTOR4F_D(x)[0] == SCM_VECTOR4F_D(y)[0]
            && SCM_VECTOR4F_D(x)[1] == SCM_VECTOR4F_D(y)[1]
            && SCM_VECTOR4F_D(x)[2] == SCM_VECTOR4F_D(y)[2]
            && SCM_VECTOR4F_D(x)[3] == SCM_VECTOR4F_D(y)[3]) {
            return 0;
        } else {
            return -1;
        }
    } else {
        Scm_Error("can't order %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_Vector4fClass, vec_print, vec_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakeVector4f(float x, float y, float z, float w)
{
    ScmVector4f *v = SCM_NEW(ScmVector4f);
    SCM_SET_CLASS(v, SCM_CLASS_VECTOR4F);
    v->v = ALLOC_FV(4);
    SCM_VECTOR4F_D(v)[0] = x; SCM_VECTOR4F_D(v)[1] = y;
    SCM_VECTOR4F_D(v)[2] = z; SCM_VECTOR4F_D(v)[3] = w;
    return SCM_OBJ(v);
}

ScmObj Scm_MakeVector4fvShared(float d[])
{
    ScmVector4f *v;
    v = SCM_NEW(ScmVector4f);
    SCM_SET_CLASS(v, SCM_CLASS_VECTOR4F);
    v->v = d;
    return SCM_OBJ(v);
}

ScmObj Scm_MakeVector4fv(const float *d)
{
    if (d) return Scm_MakeVector4f(d[0], d[1], d[2], d[3]);
    else   return Scm_MakeVector4f(0.0, 0.0, 0.0, 0.0);
}

/* common routine for list->vector4f and list->point4f */

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
        d[3] = init3;
    }
    if (SCM_NULLP(lp)) return;
  badlist:
    Scm_Error("list of 3 or 4 real numbers required, but got %S", l);
}

ScmObj Scm_ListToVector4f(ScmObj l)
{
    float d[4];
    list2vec(l, d, 0.0);
    return Scm_MakeVector4fv(d);
}

ScmObj Scm_Vector4fToList(const ScmVector4f *v)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_VECTOR4F_D(v)[0]),
                     Scm_MakeFlonum(SCM_VECTOR4F_D(v)[1]),
                     Scm_MakeFlonum(SCM_VECTOR4F_D(v)[2]),
                     Scm_MakeFlonum(SCM_VECTOR4F_D(v)[3]));
}

float Scm_Vector4fDot(const ScmVector4f *p, const ScmVector4f *q)
{
    return (SCM_VECTOR4F_DOTV(SCM_VECTOR4F_D(p), SCM_VECTOR4F_D(q)));
}

float Scm_Vector4fDotv(const float *p, const float *q)
{
    return SCM_VECTOR4F_DOTV(p, q);
}

ScmObj Scm_Vector4fCross(const ScmVector4f *p, const ScmVector4f *q)
{
    float r[4];
    SCM_VECTOR4F_CROSSV(r, SCM_VECTOR4F_D(p), SCM_VECTOR4F_D(q));
    return Scm_MakeVector4fv(r);
}

void Scm_Vector4fCrossv(float *r, const float *p, const float *q)
{
    SCM_VECTOR4F_CROSSV(r, p, q);
}

ScmObj Scm_Vector4fNormalize(const ScmVector4f *p)
{
    float r[4];
    r[0] = SCM_VECTOR4F_D(p)[0];
    r[1] = SCM_VECTOR4F_D(p)[1];
    r[2] = SCM_VECTOR4F_D(p)[2];
    r[3] = SCM_VECTOR4F_D(p)[3];
    SCM_VECTOR4F_NORMALIZEV(r);
    return Scm_MakeVector4fv(r);
}

void Scm_Vector4fNormalizev(float *p)
{
    SCM_VECTOR4F_NORMALIZEV(p);
}

ScmObj Scm_Vector4fNormalizeX(ScmVector4f *p)
{
    SCM_VECTOR4F_NORMALIZEV(SCM_VECTOR4F_D(p));
    return SCM_OBJ(p);
}

ScmObj Scm_Vector4fAdd(const ScmVector4f *p, const ScmVector4f *q)
{
    float r[4];
    SCM_VECTOR4F_ADDV(r, SCM_VECTOR4F_D(p), SCM_VECTOR4F_D(q));
    return Scm_MakeVector4fv(r);
}

void Scm_Vector4fAddv(float *r, const float *p, const float *q)
{
    SCM_VECTOR4F_ADDV(r, p, q);
}

ScmObj Scm_Vector4fSub(const ScmVector4f *p, const ScmVector4f *q)
{
    float r[4];
    SCM_VECTOR4F_SUBV(r, SCM_VECTOR4F_D(p), SCM_VECTOR4F_D(q));
    return Scm_MakeVector4fv(r);
}

void Scm_Vector4fSubv(float *r, const float *p, const float *q)
{
    SCM_VECTOR4F_SUBV(r, p, q);
}

/*=============================================================
 * VectorArray
 */

static void vec_array_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    ScmVector4fArray *va = SCM_VECTOR4F_ARRAY(obj);
    int len = SCM_VECTOR4F_ARRAY_SIZE(va), i;
    Scm_Printf(out, "#,(vector4f-array %d ", len);
    for (i = 0; i < len; i++) {
        float *z = Scm_Vector4fArrayRefv(va, i);
        Scm_Printf(out, "(%g %g %g %g) ", z[0], z[1], z[2], z[3]);
    }
    Scm_Printf(out, ")");
}

static int vec_array_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        int i, len = SCM_VECTOR4F_ARRAY_SIZE(x);
        float *p = SCM_VECTOR4F_ARRAY_D(x), *q = SCM_VECTOR4F_ARRAY_D(y);
        if (len != SCM_VECTOR4F_ARRAY_SIZE(y)) return 0;
        for (i=0; i<len*4; i++) {
            if (*p++ != *q++) return 0;
        }
        return -1;
    } else {
        Scm_Error("can't order %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_Vector4fArrayClass,
                         vec_array_print, vec_array_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakeVector4fArrayv(int nvecs, const float *init)
{
    int i, j;
    ScmVector4fArray *a;
    SCM_ASSERT(nvecs >= 0);
    a = SCM_NEW(ScmVector4fArray);
    SCM_SET_CLASS(a, SCM_CLASS_VECTOR4F_ARRAY);
    a->size = nvecs;
    a->v = ALLOC_FV(nvecs*4);
    if (init) {
        for (i=0;i<nvecs;i++) {
            SCM_VECTOR4F_ARRAY_SET(a, i, init[0], init[1], init[2], init[3]);
        }
    } else {
        for (i=0;i<nvecs*4;i++) SCM_VECTOR4F_ARRAY_D(a)[i] = 0.0;
    }
    return SCM_OBJ(a);
}

ScmObj Scm_MakeVector4fArrayV(ScmF32Vector *fv)
{
    int size;
    ScmVector4fArray *a;
    size = SCM_F32VECTOR_SIZE(fv);
    if (size % 4 != 0) {
        Scm_Error("f32vector size must be multiple of 4, but got %S", fv);
    }
    a = SCM_NEW(ScmVector4fArray);
    SCM_SET_CLASS(a, SCM_CLASS_VECTOR4F_ARRAY);
    a->size = size/4;
    a->v = SCM_F32VECTOR_ELEMENTS(fv); /* share the storage */
    return SCM_OBJ(a);
}

ScmObj Scm_Vector4fArrayRef(const ScmVector4fArray *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_VECTOR4F_ARRAY_SIZE(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_MakeVector4fv(SCM_VECTOR4F_ARRAY_REFV(a, n));
}

ScmObj Scm_Vector4fArrayRefShared(ScmVector4fArray *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_VECTOR4F_ARRAY_SIZE(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_MakeVector4fvShared(SCM_VECTOR4F_ARRAY_REFV(a, n));
}

float *Scm_Vector4fArrayRefv(ScmVector4fArray *a, int n)
{
    return SCM_VECTOR4F_ARRAY_REFV(a, n);
}

void Scm_Vector4fArraySet(ScmVector4fArray *a, int n, ScmVector4f *v)
{
    Scm_Vector4fArraySetv(a, n, SCM_VECTOR4F_D(v));
}

void Scm_Vector4fArraySetv(ScmVector4fArray *a, int n, float d[])
{
    if (n < 0 || n >= SCM_VECTOR4F_ARRAY_SIZE(a)) {
        Scm_Error("index out of range");
    }
    SCM_VECTOR4F_ARRAY_SET(a, n, d[0], d[1], d[2], d[3]);
}

/*=============================================================
 * Pointf - is just a vector with different default value.
 */
static void point_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(point4f %g %g %g %g)",
               SCM_VECTOR4F_D(obj)[0],
               SCM_VECTOR4F_D(obj)[1],
               SCM_VECTOR4F_D(obj)[2],
               SCM_VECTOR4F_D(obj)[3]);
}

#define point_compare vec_compare

SCM_DEFINE_BUILTIN_CLASS(Scm_Point4fClass, point_print, point_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakePoint4f(float x, float y, float z, float w)
{
    ScmPoint4f *v = SCM_NEW(ScmPoint4f);
    SCM_SET_CLASS(v, SCM_CLASS_POINT4F);
    v->v = ALLOC_FV(4);
    SCM_POINT4F_D(v)[0] = x; SCM_POINT4F_D(v)[1] = y;
    SCM_POINT4F_D(v)[2] = z; SCM_POINT4F_D(v)[3] = w;
    return SCM_OBJ(v);
}

ScmObj Scm_MakePoint4fv(const float *d)
{
    if (d) return Scm_MakePoint4f(d[0], d[1], d[2], d[3]);
    else   return Scm_MakePoint4f(0.0, 0.0, 0.0, 0.0);
}

ScmObj Scm_MakePoint4fvShared(float d[])
{
    ScmPoint4f *v;
    v = SCM_NEW(ScmPoint4f);
    SCM_SET_CLASS(v, SCM_CLASS_POINT4F);
    v->v = d;
    return SCM_OBJ(v);
}

ScmObj Scm_ListToPoint4f(ScmObj l)
{
    float d[4];
    list2vec(l, d, 1.0);
    return Scm_MakePoint4fv(d);
}

ScmObj Scm_Point4fToList(const ScmPoint4f *p)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_POINT4F_D(p)[0]),
                     Scm_MakeFlonum(SCM_POINT4F_D(p)[1]),
                     Scm_MakeFlonum(SCM_POINT4F_D(p)[2]),
                     Scm_MakeFlonum(SCM_POINT4F_D(p)[3]));
}

ScmObj Scm_Point4fAdd(const ScmPoint4f *p, const ScmVector4f *q)
{
    float r[4];
    SCM_VECTOR4F_ADDV(r, SCM_POINT4F_D(p), SCM_VECTOR4F_D(q));
    return Scm_MakePoint4fv(r);
}

ScmObj Scm_Point4fSub(const ScmVector4f *p, const ScmObj q)
{
    float r[4];
    if (SCM_POINT4FP(q)) {
        SCM_VECTOR4F_SUBV(r, SCM_POINT4F_D(p), SCM_POINT4F_D(q));
        return Scm_MakeVector4fv(r);
    }
    if (SCM_VECTOR4FP(q)) {
        SCM_VECTOR4F_SUBV(r, SCM_POINT4F_D(p), SCM_VECTOR4F_D(q));
        return Scm_MakePoint4fv(r);
    }
    Scm_Error("<point4f> or <vector4f> required, but got %S", q);
    return SCM_UNDEFINED;
}

/*=============================================================
 * Point4fArray
 */

static void point_array_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    ScmPoint4fArray *va = SCM_POINT4F_ARRAY(obj);
    int len = SCM_POINT4F_ARRAY_SIZE(va), i;
    Scm_Printf(out, "#,(point4f-array %d ", len);
    for (i = 0; i < len; i++) {
        float *z = Scm_Point4fArrayRefv(va, i);
        Scm_Printf(out, "(%g %g %g %g) ", z[0], z[1], z[2], z[3]);
    }
    Scm_Printf(out, ")");
}

#define point_array_compare vec_array_compare

SCM_DEFINE_BUILTIN_CLASS(Scm_Point4fArrayClass, point_array_print,
                         point_array_compare, NULL, NULL, sequenceCPL);

ScmObj Scm_MakePoint4fArrayv(int len, const float *init)
{
    int i, j;
    ScmPoint4fArray *a;
    SCM_ASSERT(len >= 0);
    a = SCM_NEW(ScmPoint4fArray);
    SCM_SET_CLASS(a, SCM_CLASS_POINT4F_ARRAY);
    a->size = len;
    a->v = ALLOC_FV(len*4);
    if (init) {
        for (i=0;i<len;i++) {
            SCM_POINT4F_ARRAY_SET(a, i, init[0], init[1], init[2], init[3]);
        }
    } else {
        for (i=0;i<len*4;i++) SCM_POINT4F_ARRAY_D(a)[i] = 0.0;
    }
    return SCM_OBJ(a);
}

ScmObj Scm_MakePoint4fArrayV(ScmF32Vector *fv)
{
    int size;
    ScmPoint4fArray *a;
    size = SCM_F32VECTOR_SIZE(fv);
    if (size % 4 != 0) {
        Scm_Error("f32vector size must be multiple of 4, but got %S", fv);
    }
    a = SCM_NEW(ScmPoint4fArray);
    SCM_SET_CLASS(a, SCM_CLASS_POINT4F_ARRAY);
    a->size = size/4;
    a->v = SCM_F32VECTOR_ELEMENTS(fv);
    return SCM_OBJ(a);
}

ScmObj Scm_Point4fArrayRef(const ScmPoint4fArray *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_POINT4F_ARRAY_SIZE(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_MakePoint4fv(SCM_POINT4F_ARRAY_REFV(a, n));
}

ScmObj Scm_Point4fArrayRefShared(ScmPoint4fArray *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_POINT4F_ARRAY_SIZE(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_MakePoint4fvShared(SCM_POINT4F_ARRAY_REFV(a, n));
}

float *Scm_Point4fArrayRefv(ScmPoint4fArray *a, int n)
{
    return SCM_POINT4F_ARRAY_REFV(a, n);
}

void Scm_Point4fArraySet(ScmPoint4fArray *a, int n, ScmPoint4f *v)
{
    if (n < 0 || n >= SCM_POINT4F_ARRAY_SIZE(a)) {
        Scm_Error("index out of range");
    }
    SCM_POINT4F_ARRAY_SET(a, n,
                         SCM_POINT4F_D(v)[0],
                         SCM_POINT4F_D(v)[1],
                         SCM_POINT4F_D(v)[2],
                         SCM_POINT4F_D(v)[3]);
}

void Scm_Point4fArraySetv(ScmPoint4fArray *a, int n, float d[])
{
    SCM_POINT4F_ARRAY_SET(a, n, d[0], d[1], d[2], d[3]);
}

/*=============================================================
 * Matrix
 */

static void mat_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    int i;
    ScmMatrix4f *m = SCM_MATRIX4F(obj);
    Scm_Printf(out, "#,(matrix4f");
    for (i=0; i<16; i++) {
        Scm_Printf(out, " %g", SCM_MATRIX4F_D(m)[i]);
    }
    Scm_Printf(out, ")");
}

static int mat_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        int i;
        float *p = SCM_MATRIX4F_D(x), *q = SCM_MATRIX4F_D(y);
        for (i=0; i<16; i++) {
            if (*p++ != *q++) return -1;
        }
        return 0;
    } else {
        Scm_Error("can't order matrix %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_Matrix4fClass, mat_print, mat_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakeMatrix4fv(const float *d)
{
    ScmMatrix4f *m = SCM_NEW(ScmMatrix4f);
    int i;
    SCM_SET_CLASS(m, SCM_CLASS_MATRIX4F);
    m->v = ALLOC_FV(16);
    if (d == NULL) {
        for (i=0; i<16; i++) SCM_MATRIX4F_D(m)[i] = ((i/4==i%4)? 1.0 : 0.0);
    } else {
        for (i=0; i<16; i++) SCM_MATRIX4F_D(m)[i] = d[i];
    }
    return SCM_OBJ(m);
}

ScmObj Scm_MakeMatrix4fvShared(float *d)
{
    ScmMatrix4f *m = SCM_NEW(ScmMatrix4f);
    SCM_MATRIX4F_D(m) = d;
    return SCM_OBJ(m);
}

ScmObj Scm_ListToMatrix4f(ScmObj l)
{
    int i;
    ScmObj lp = l;
    float d[16];
    for (i=0; i<16; i++, lp = SCM_CDR(lp)) {
        if (!SCM_PAIRP(lp)) goto badlist;
        if (!SCM_REALP(SCM_CAR(lp))) goto badlist;
        d[i] = (float)Scm_GetDouble(SCM_CAR(lp));
    }
    if (!SCM_NULLP(lp)) goto badlist;
    return Scm_MakeMatrix4fv(d);
  badlist:
    Scm_Error("list of 16 real numbers required, but got %S", l);
    return SCM_UNDEFINED;       /* dummy */
}

ScmObj Scm_Matrix4fToList(const ScmMatrix4f *m)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    int i;
    float *p = SCM_MATRIX4F_D(m);
    for (i=0; i<16; i++) {
        SCM_APPEND1(h, t, Scm_MakeFlonum((double)p[i]));
    }
    return h;
}

/* Matrix X Matrix */
void Scm_Matrix4fMulMatrix4fv(float *r, const float *p, const float *q)
{
    r[0]  = p[0]*q[0]+p[4]*q[1]+p[8]*q[2]+p[12]*q[3];
    r[1]  = p[1]*q[0]+p[5]*q[1]+p[9]*q[2]+p[13]*q[3];
    r[2]  = p[2]*q[0]+p[6]*q[1]+p[10]*q[2]+p[14]*q[3];
    r[3]  = p[3]*q[0]+p[7]*q[1]+p[11]*q[2]+p[15]*q[3];
    r[4]  = p[0]*q[4]+p[4]*q[5]+p[8]*q[6]+p[12]*q[7];
    r[5]  = p[1]*q[4]+p[5]*q[5]+p[9]*q[6]+p[13]*q[7];
    r[6]  = p[2]*q[4]+p[6]*q[5]+p[10]*q[6]+p[14]*q[7];
    r[7]  = p[3]*q[4]+p[7]*q[5]+p[11]*q[6]+p[15]*q[7];
    r[8]  = p[0]*q[8]+p[4]*q[9]+p[8]*q[10]+p[12]*q[11];
    r[9]  = p[1]*q[8]+p[5]*q[9]+p[9]*q[10]+p[13]*q[11];
    r[10] = p[2]*q[8]+p[6]*q[9]+p[10]*q[10]+p[14]*q[11];
    r[11] = p[3]*q[8]+p[7]*q[9]+p[11]*q[10]+p[15]*q[11];
    r[12] = p[0]*q[12]+p[4]*q[13]+p[8]*q[14]+p[12]*q[15];
    r[13] = p[1]*q[12]+p[5]*q[13]+p[9]*q[14]+p[13]*q[15];
    r[14] = p[2]*q[12]+p[6]*q[13]+p[10]*q[14]+p[14]*q[15];
    r[15] = p[3]*q[12]+p[7]*q[13]+p[11]*q[14]+p[15]*q[15];
}

ScmObj Scm_Matrix4fMulMatrix4f(const ScmMatrix4f *p, const ScmMatrix4f *q)
{
    ScmMatrix4f *r = SCM_MATRIX4F(Scm_MakeMatrix4fv(NULL));
    Scm_Matrix4fMulMatrix4fv(SCM_MATRIX4F_D(r), SCM_MATRIX4F_D(p), SCM_MATRIX4F_D(q));
    return SCM_OBJ(r);
}

/* Matrix X Vector */
void Scm_Matrix4fMulVector4fv(float *r, const float *m, const float *v)
{
    r[0] = m[0]*v[0]+m[4]*v[1]+m[8]*v[2]+m[12]*v[3];
    r[1] = m[1]*v[0]+m[5]*v[1]+m[9]*v[2]+m[13]*v[3];
    r[2] = m[2]*v[0]+m[6]*v[1]+m[10]*v[2]+m[14]*v[3];
    r[3] = m[3]*v[0]+m[7]*v[1]+m[11]*v[2]+m[15]*v[3];
}

ScmObj Scm_Matrix4fMulVector4f(const ScmMatrix4f *m, const ScmVector4f *v)
{
    ScmVector4f *r = SCM_VECTOR4F(Scm_MakeVector4fv(NULL));
    Scm_Matrix4fMulVector4fv(SCM_MATRIX4F_D(r), SCM_MATRIX4F_D(m), SCM_MATRIX4F_D(v));
    return SCM_OBJ(r);
}

ScmObj Scm_Matrix4fMulPoint4f(const ScmMatrix4f *m, const ScmPoint4f *p)
{
    ScmPoint4f *r = SCM_POINT4F(Scm_MakePoint4fv(NULL));
    Scm_Matrix4fMulVector4fv(SCM_MATRIX4F_D(r), SCM_MATRIX4F_D(m), SCM_MATRIX4F_D(p));
    return SCM_OBJ(r);
}

void   Scm_Matrix4fScalev(float *r, double f)
{
    int i;
    for (i=0; i<16; i++) r[i] *= f;
}

ScmObj Scm_Matrix4fScale(const ScmMatrix4f *m, double f)
{
    ScmMatrix4f *r = SCM_MATRIX4F(Scm_MakeMatrix4fv(SCM_MATRIX4F_D(m)));
    Scm_Matrix4fScalev(SCM_MATRIX4F_D(r), f);
    return SCM_OBJ(r);
}

void   Scm_Matrix4fTransposev(float *r, const float *m)
{
    r[0] = m[0];  r[4] = m[1];  r[8] = m[2];  r[12]= m[3];
    r[1] = m[4];  r[5] = m[5];  r[9] = m[6];  r[13]= m[7];
    r[2] = m[8];  r[6] = m[9];  r[10]= m[10]; r[14]= m[11];
    r[3] = m[12]; r[7] = m[13]; r[11]= m[14]; r[15]= m[15];
}

/*
 * Transformation and matrices.
 */
void Scm_TRSToMatrix4fv(float *m, const float *t,
                        const float *v, float phi,
                        const float *s)
{
    float cosp = cos(phi), sinp = sin(phi);
    float vxx = v[0]*v[0], vyy = v[1]*v[1], vzz = v[2]*v[2];
    float vxy = v[0]*v[1], vyz = v[1]*v[2], vzx = v[2]*v[0];

    m[0]  = s[0] * (cosp + (1.-cosp)*vxx);
    m[1]  = (1.-cosp)*vxy + v[2]*sinp;
    m[2]  = (1.-cosp)*vzx - v[1]*sinp;
    m[3]  = 0.0;
    
    m[4]  = (1.-cosp)*vxy - v[2]*sinp;
    m[5]  = s[1] * (cosp + (1.-cosp)*vyy);
    m[6]  = (1.-cosp)*vyz + v[0]*sinp;
    m[7]  = 0.0;
    
    m[8]  = (1.-cosp)*vzx + v[1]*sinp;
    m[9]  = (1.-cosp)*vyz - v[0]*sinp;
    m[10] = s[2] * (cosp + (1.-cosp)*vzz);
    m[11] = 0.0;
    
    m[12] = t[0];
    m[13] = t[1];
    m[14] = t[2];
    m[15] = 1.0;
}

void Scm_TQSToMatrix4fv(float *m, const float *t,
                        const float *q,
                        const float *s)
{
    float x2 = q[0]*q[0], y2 = q[1]*q[1], z2 = q[2]*q[2];
    float xy = q[0]*q[1], yz = q[1]*q[2], zx = q[2]*q[0];
    float xw = q[0]*q[3], yw = q[1]*q[3], zw = q[2]*q[3];

    m[0]  = s[0] * (1-2*(y2+z2));
    m[1]  = 2*(xy+zw);
    m[2]  = 2*(zx-yw);
    m[3]  = 0.0;
    
    m[4]  = 2*(xy-zw);
    m[5]  = s[1] * (1-2*(z2+x2));
    m[6]  = 2*(yz+xw);
    m[7]  = 0.0;
    
    m[8]  = 2*(zx+yw);
    m[9]  = 2*(yz-xw);
    m[10] = s[2] * (1-2*(x2+y2));
    m[11] = 0.0;
    
    m[12] = t[0];
    m[13] = t[1];
    m[14] = t[2];
    m[15] = 1.0;
}


void Scm_TranslationToMatrix4fv(float *m, const float *t)
{
    m[0] = 1.0;  m[4] = 0.0;  m[8] = 0.0;  m[12] = t[0];
    m[1] = 0.0;  m[5] = 1.0;  m[9] = 0.0;  m[13] = t[1];
    m[2] = 0.0;  m[6] = 0.0;  m[10] = 1.0; m[14] = t[2];
    m[3] = 0.0;  m[7] = 1.0;  m[11] = 0.0; m[15] = 1.0;
}

void Scm_RotationToMatrix4fv(float *m, const float *v, float phi)
{
    static const float t[3] = { 0.0, 0.0, 0.0 };
    static const float s[3] = { 1.0, 1.0, 1.0 };
    Scm_TRSToMatrix4fv(m, t, v, phi, s);
}

void Scm_ScaleToMatrix4fv(float *m, const float *s)
{
    m[0] = s[0]; m[4] = 0.0; m[8] = 0.0; m[12] = 0.0;
    m[1] = 0.0; m[5] = s[1]; m[9] = 0.0; m[13] = 0.0;
    m[2] = 0.0; m[6] = 0.0; m[10] = s[2]; m[14] = 0.0;
    m[3] = 0.0; m[7] = 0.0; m[11] = 0.0; m[15] = 1.0;
}

/*=============================================================
 * Quaternion
 */
static void quat_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(quatf %g %g %g %g)",
               SCM_QUATF_D(obj)[0],
               SCM_QUATF_D(obj)[1],
               SCM_QUATF_D(obj)[2],
               SCM_QUATF_D(obj)[3]);
}

static int quat_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        if (SCM_QUATF_D(x)[0] == SCM_QUATF_D(y)[0]
            && SCM_QUATF_D(x)[1] == SCM_QUATF_D(y)[1]
            && SCM_QUATF_D(x)[2] == SCM_QUATF_D(y)[2]
            && SCM_QUATF_D(x)[3] == SCM_QUATF_D(y)[3]) {
            return 0;
        } else {
            return -1;
        }
    } else {
        Scm_Error("can't order quat %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_QuatfClass, quat_print, quat_compare,
                         NULL, NULL, sequenceCPL);

/*
 * Constructors and converters
 */
ScmObj Scm_MakeQuatf(float x, float y, float z, float w)
{
    ScmQuatf *v = SCM_NEW(ScmQuatf);
    SCM_SET_CLASS(v, SCM_CLASS_QUATF);
    SCM_QUATF_D(v) = ALLOC_FV(4);
    SCM_QUATF_D(v)[0] = x;
    SCM_QUATF_D(v)[1] = y;
    SCM_QUATF_D(v)[2] = z;
    SCM_QUATF_D(v)[3] = w;
    return SCM_OBJ(v);
}

ScmObj Scm_MakeQuatfv(const float d[])
{
    if (d) return Scm_MakeQuatf(d[0], d[1], d[2], d[3]);
    else   return Scm_MakeQuatf(0.0, 0.0, 0.0, 1.0);
}

ScmObj Scm_MakeQuatfvShared(float d[])
{
    ScmQuatf *v = SCM_NEW(ScmQuatf);
    SCM_SET_CLASS(v, SCM_CLASS_QUATF);
    SCM_QUATF_D(v) = d;
    return SCM_OBJ(v);
}

ScmObj Scm_ListToQuatf(ScmObj l)
{
    int i;
    float d[4];
    ScmObj lp = l;
    for (i=0; i<4; i++, lp = SCM_CDR(lp)) {
        if (!SCM_PAIRP(lp)) goto badlist;
        if (!SCM_REALP(SCM_CAR(lp))) goto badlist;
        d[i] = (float)Scm_GetDouble(SCM_CAR(lp));
    }
    return Scm_MakeQuatfv(d);
  badlist:
    Scm_Error("list of 3 or 4 real numbers required, but got %S", l);
    return SCM_UNDEFINED;
}

ScmObj Scm_QuatfToList(const ScmQuatf *p)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_QUATF_D(p)[0]),
                     Scm_MakeFlonum(SCM_QUATF_D(p)[1]),
                     Scm_MakeFlonum(SCM_QUATF_D(p)[2]),
                     Scm_MakeFlonum(SCM_QUATF_D(p)[3]));
}

void Scm_QuatfAddv(float *r, const float *p, const float *q)
{
    SCM_VECTOR4F_OP(i, r[i] = p[i] + q[i]);
}

ScmObj Scm_QuatfAdd(const ScmQuatf *p, const ScmQuatf *q)
{
    float r[4];
    SCM_VECTOR4F_OP(i, r[i] = SCM_QUATF_D(p)[i] + SCM_QUATF_D(q)[i]);
    return Scm_MakeQuatfv(r);
}

void Scm_QuatfSubv(float *r, const float *p, const float *q)
{
    SCM_VECTOR4F_OP(i, r[i] = p[i] - q[i]);
}

ScmObj Scm_QuatfSub(const ScmQuatf *p, const ScmQuatf *q)
{
    float r[4];
    SCM_VECTOR4F_OP(i, r[i] = SCM_QUATF_D(p)[i] - SCM_QUATF_D(q)[i]);
    return Scm_MakeQuatfv(r);
}

void Scm_QuatfMulv(float *r, const float *p, const float *q)
{
    r[0] = p[0]*q[3]+p[1]*q[2]-p[2]*q[1]+p[3]*q[0];
    r[1] = p[1]*q[3]+p[2]*q[0]-p[0]*q[2]+p[3]*q[1];
    r[2] = p[2]*q[3]+p[0]*q[1]-p[1]*q[0]+p[3]*q[2];
    r[3] = -p[0]*q[0]-p[1]*q[1]-p[2]*q[2]+p[3]*q[3];
}

ScmObj Scm_QuatfMul(const ScmQuatf *p, const ScmQuatf *q)
{
    float r[4];
    Scm_QuatfMulv(r, SCM_QUATF_D(p), SCM_QUATF_D(q));
    return Scm_MakeQuatfv(r);
}

ScmObj Scm_QuatfNormalize(const ScmQuatf *p)
{
    float r[4];
    r[0] = SCM_QUATF_D(p)[0];
    r[1] = SCM_QUATF_D(p)[1];
    r[2] = SCM_QUATF_D(p)[2];
    r[3] = SCM_QUATF_D(p)[3];
    SCM_QUATF_NORMALIZEV(r);
    return Scm_MakeQuatfv(r);
}

ScmObj Scm_QuatfNormalizeX(ScmQuatf *p)
{
    SCM_QUATF_NORMALIZEV(SCM_QUATF_D(p));
    return SCM_OBJ(p);
}

void Scm_QuatfToMatrixv(float m[], const float q[])
{
    float x2 = q[0]*q[0], y2 = q[1]*q[1], z2 = q[2]*q[2];
    float xy = q[0]*q[1], yz = q[1]*q[2], zx = q[2]*q[0];
    float xw = q[0]*q[3], yw = q[1]*q[3], zw = q[2]*q[3];
    m[0] = 1-2*(y2+z2); m[4] = 2*(xy-zw); m[8] = 2*(zx+yw); m[12] = 0;
    m[1] = 2*(xy+zw); m[5] = 1-2*(z2+x2); m[9] = 2*(yz-xw); m[13] = 0;
    m[2] = 2*(zx-yw); m[6] = 2*(yz+xw); m[10] = 1-2*(x2+y2); m[14] = 0;
    m[3] = 0; m[7] = 0; m[11] = 0; m[15] = 1;
}

void Scm_QuatfSlerp(float r[], const float p[], const float q[], float t)
{
    double cosphi = p[0]*q[0]+p[1]*q[1]+p[2]*q[2]+p[3]*q[3], phi, sinphi;
    float cp, cq;
        
    phi = acos(cosphi);
    sinphi = sin(phi);
    if (sinphi < 1.0e-5 && sinphi > -1.0e-5) {
        cp = 1.0 - t; cq = t;
    } else {
        cp = sin(phi*(1.0-t))/sinphi; cq = sin(phi*t)/sinphi;
    }
    SCM_VECTOR4F_OP(i, r[i] = cp*p[i] + cq*q[i]);
}

/*=============================================================
 * Initialization
 */
extern void Scm_Init_math3d_lib(ScmModule *mod);

void Scm_Init_gauche_math3d(void)
{
    ScmModule *mod;
    SCM_INIT_EXTENSION(gauche_math3d);
    mod = SCM_MODULE(SCM_FIND_MODULE("gl.math3d", TRUE));
    Scm_InitBuiltinClass(&Scm_Vector4fClass, "<vector4f>",
                         NULL, sizeof(ScmVector4f)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_Vector4fArrayClass, "<vector4f-array>",
                         NULL, sizeof(ScmVector4fArray)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_Point4fClass, "<point4f>",
                         NULL, sizeof(ScmPoint4f)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_Point4fArrayClass, "<point4f-array>",
                         NULL, sizeof(ScmPoint4fArray)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_Matrix4fClass, "<matrix4f>",
                         NULL, sizeof(ScmMatrix4f)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_QuatfClass, "<quatf>",
                         NULL, sizeof(ScmQuatf)/sizeof(ScmObj),
                         mod);
    Scm_Init_math3d_lib(mod);
}

