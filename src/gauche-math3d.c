/*
 * gauche-math3d.c - 3D vector and matrix arithmetics
 *
 *  Copyright(C) 2002-2003 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: gauche-math3d.c,v 1.21 2003-01-30 05:50:12 shirok Exp $
 */

#include <math.h>
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

ScmObj Scm_Vector4fSetv(ScmVector4f *v, float *d)
{
    float *fp = SCM_VECTOR4F_D(v);
    fp[0] = d[0]; fp[1] = d[1]; fp[2] = d[2]; fp[3] = d[3];
    return SCM_OBJ(v);
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
    int i;
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
    else   return Scm_MakePoint4f(0.0, 0.0, 0.0, 1.0);
}

ScmObj Scm_MakePoint4fvShared(float d[])
{
    ScmPoint4f *v;
    v = SCM_NEW(ScmPoint4f);
    SCM_SET_CLASS(v, SCM_CLASS_POINT4F);
    v->v = d;
    return SCM_OBJ(v);
}

ScmObj Scm_Point4fSetv(ScmPoint4f *v, float *d)
{
    float *fp = SCM_POINT4F_D(v);
    fp[0] = d[0]; fp[1] = d[1]; fp[2] = d[2]; fp[3] = d[3];
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
    int i;
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
        Scm_Matrix4fSetIdentityv(SCM_MATRIX4F_D(m));
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

ScmObj Scm_Matrix4fSetv(ScmMatrix4f *m, float *d)
{
    int i;
    float *fp = SCM_MATRIX4F_D(m);
    for (i=0; i<16; i++) fp[i] = d[i];
    return SCM_OBJ(m);
}

void Scm_Matrix4fSetIdentityv(float *p)
{
    *p++ = 1.0; *p++ = 0.0; *p++ = 0.0; *p++ = 0.0;
    *p++ = 0.0; *p++ = 1.0; *p++ = 0.0; *p++ = 0.0;
    *p++ = 0.0; *p++ = 0.0; *p++ = 1.0; *p++ = 0.0;
    *p++ = 0.0; *p++ = 0.0; *p++ = 0.0; *p   = 1.0;
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
 * Determinant and inverse
 */
#define DET3(m00, m01, m02, m10, m11, m12, m20, m21, m22)        \
  ((m00)*(m11)*(m22) + (m10)*(m21)*(m02) + (m20)*(m01)*(m12)     \
   - (m00)*(m21)*(m12) - (m10)*(m01)*(m22) - (m20)*(m11)*(m02))
#define M00 m[0]
#define M01 m[4]
#define M02 m[8]
#define M03 m[12]
#define M10 m[1]
#define M11 m[5]
#define M12 m[9]
#define M13 m[13]
#define M20 m[2]
#define M21 m[6]
#define M22 m[10]
#define M23 m[14]
#define M30 m[3]
#define M31 m[7]
#define M32 m[11]
#define M33 m[15]

float Scm_Matrix4fDeterminantv(const float *m)
{
    float d00, d10, d20, d30;
    d00 = M00 * DET3(M11, M12, M13, M21, M22, M23, M31, M32, M33);
    d10 = M10 * DET3(M01, M02, M03, M21, M22, M23, M31, M32, M33);
    d20 = M20 * DET3(M01, M02, M03, M11, M12, M13, M31, M32, M33);
    d30 = M30 * DET3(M01, M02, M03, M11, M12, M13, M21, M22, M23);
    return d00 - d10 + d20 - d30;
}

/* Returns FALSE if M is singular */
int   Scm_Matrix4fInversev(float *r, const float *m)
{
    float det = Scm_Matrix4fDeterminantv(m);
    if (det == 0.0) return FALSE;
    r[0]  =  DET3(M11, M12, M13, M21, M22, M23, M31, M32, M33)/det; /*d00*/
    r[1]  = -DET3(M10, M12, M13, M20, M22, M23, M30, M32, M33)/det; /*d01*/
    r[2]  =  DET3(M10, M11, M13, M20, M21, M23, M30, M31, M33)/det; /*d02*/
    r[3]  = -DET3(M10, M11, M12, M20, M21, M22, M30, M31, M32)/det; /*d03*/

    r[4]  = -DET3(M01, M02, M03, M21, M22, M23, M31, M32, M33)/det; /*d10*/
    r[5]  =  DET3(M00, M02, M03, M20, M22, M23, M30, M32, M33)/det; /*d11*/
    r[6]  = -DET3(M00, M01, M03, M20, M21, M23, M30, M31, M33)/det; /*d12*/
    r[7]  =  DET3(M00, M01, M02, M20, M21, M22, M30, M31, M32)/det; /*d13*/

    r[8]  =  DET3(M01, M02, M03, M11, M12, M13, M31, M32, M33)/det; /*d20*/
    r[9]  = -DET3(M00, M02, M03, M10, M12, M13, M30, M32, M33)/det; /*d21*/
    r[10] =  DET3(M00, M01, M03, M10, M11, M13, M30, M31, M33)/det; /*d22*/
    r[11] = -DET3(M00, M01, M02, M10, M11, M12, M30, M31, M32)/det; /*d23*/

    r[12] = -DET3(M01, M02, M03, M11, M12, M13, M21, M22, M23)/det; /*d30*/
    r[13] =  DET3(M00, M02, M03, M10, M12, M13, M20, M22, M23)/det; /*d31*/
    r[14] = -DET3(M00, M01, M03, M10, M11, M13, M20, M21, M23)/det; /*d32*/
    r[15] =  DET3(M00, M01, M02, M10, M11, M12, M20, M21, M22)/det; /*d33*/
    return TRUE;
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
    m[3] = 0.0;  m[7] = 0.0;  m[11] = 0.0; m[15] = 1.0;
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

/* Euler angles -> rotation matrix.  Rotation order is fixed. */
void Scm_EulerToMatrix4fv(float m[], float x, float y, float z, int order)
{
    float cx = cosf(x), sx = sinf(x);
    float cy = cosf(y), sy = sinf(y);
    float cz = cosf(z), sz = sinf(z);

    switch (order) {
        /* Nobody wants to write down these expressions by hand.
           The code in this switch statement is generated by the script
           attached at the end of this source code. */
    case SCM_MATH3D_ROTATE_XYZ:
        m[0] = cz*cy;
        m[1] = sz*cy;
        m[2] = -sy;
        m[4] = cz*sy*sx + -sz*cx;
        m[5] = sz*sy*sx + cz*cx;
        m[6] = cy*sx;
        m[8] = cz*sy*cx + sz*sx;
        m[9] = sz*sy*cx + -cz*sx;
        m[10] = cy*cx;
        break;
    case SCM_MATH3D_ROTATE_XZY:
        m[0] = cy*cz;
        m[1] = sz;
        m[2] = -sy*cz;
        m[4] = -cy*sz*cx + sy*sx;
        m[5] = cz*cx;
        m[6] = sy*sz*cx + cy*sx;
        m[8] = cy*sz*sx + sy*cx;
        m[9] = -cz*sx;
        m[10] = -sy*sz*sx + cy*cx;
        break;
    case SCM_MATH3D_ROTATE_YZX:
        m[0] = cz*cy;
        m[1] = cx*sz*cy + sx*sy;
        m[2] = sx*sz*cy + -cx*sy;
        m[4] = -sz;
        m[5] = cx*cz;
        m[6] = sx*cz;
        m[8] = cz*sy;
        m[9] = cx*sz*sy + -sx*cy;
        m[10] = sx*sz*sy + cx*cy;
        break;
    case SCM_MATH3D_ROTATE_YXZ:
        m[0] = cz*cy + -sz*sx*sy;
        m[1] = sz*cy + cz*sx*sy;
        m[2] = -cx*sy;
        m[4] = -sz*cx;
        m[5] = cz*cx;
        m[6] = sx;
        m[8] = cz*sy + sz*sx*cy;
        m[9] = sz*sy + -cz*sx*cy;
        m[10] = cx*cy;
        break;
    case SCM_MATH3D_ROTATE_ZXY:
        m[0] = cy*cz + sy*sx*sz;
        m[1] = cx*sz;
        m[2] = -sy*cz + cy*sx*sz;
        m[4] = -cy*sz + sy*sx*cz;
        m[5] = cx*cz;
        m[6] = sy*sz + cy*sx*cz;
        m[8] = sy*cx;
        m[9] = -sx;
        m[10] = cy*cx;
        break;
    case SCM_MATH3D_ROTATE_ZYX:
        m[0] = cy*cz;
        m[1] = cx*sz + sx*sy*cz;
        m[2] = sx*sz + -cx*sy*cz;
        m[4] = -cy*sz;
        m[5] = cx*cz + -sx*sy*sz;
        m[6] = sx*cz + cx*sy*sz;
        m[8] = sy;
        m[9] = -sx*cy;
        m[10] = cx*cy;
        break;
    default:
        Scm_Error("bad ordering parameter for euler->matrix4f: %d", order);
    }

    /* common part */
    m[3]  = 0.0;
    m[7]  = 0.0;
    m[11] = 0.0;
    m[12] = 0.0;
    m[13] = 0.0;
    m[14] = 0.0;
    m[15] = 1.0;
}

/*
 * Matrix decomposition
 *
 *  The algorithm is taken from Thomas, Spencer W., Decomposing a Matrix
 *  Into Simple Transformations, Graphics Gems II, p. 320-323
 *
 * Decompose matrix m to translation vector T, rotation matrix R,
 * shear vector H, and scale vector S.
 * Further decomposition of rotation matrix R can be done in separate
 * function.
 *
 *  T = [tx, ty, tz, 0]
 *  S = [sx, sy, sz, 0] 
 *  H = [shear_yz, shear_zx, shear_xy, 0]
 *
 * Returns TRUE if m is non-singular, or FALSE if m is singular.
 */
int Scm_Matrix4fDecomposev(const float m[], float T[], float R[],
                           float H[], float S[])
{
    float r[3][4], temp[4], det;
    int i;

    /* Translation part is easy */
    T[0] = m[12]; T[1] = m[13]; T[2] = m[14]; T[3] = 0.0;

    /* prepare three row vectors */
    for (i=0; i<3; i++) {
        r[i][0] = m[i*4];
        r[i][1] = m[i*4+1];
        r[i][2] = m[i*4+2];
        r[i][3] = 0.0;
    }

    /* Scale X */
    S[0] = SCM_VECTOR4F_NORMV(r[0]);
    if (S[0] != 0.0) {
        SCM_VECTOR4F_OP(_, r[0][_] /= S[0]); /* normalize */
    }

    /* Shear XY */
    H[0] = SCM_VECTOR4F_DOTV(r[0], r[1]);
    r[1][0] -= H[2]*r[0][0];
    r[1][1] -= H[2]*r[0][1];
    r[1][2] -= H[2]*r[0][2];
    
    /* Scale Y */
    S[1] = SCM_VECTOR4F_NORMV(r[1]);
    if (S[1] != 0.0) {
        SCM_VECTOR4F_OP(_, r[1][_] /= S[1]); /* normalize */
        H[2] /= S[1];
    }

    /* Shear XZ */
    H[1] = SCM_VECTOR4F_DOTV(r[0], r[2]);
    r[2][0] -= H[1]*r[0][0];
    r[2][1] -= H[1]*r[0][1];
    r[2][2] -= H[1]*r[0][2];

    /* Shear YZ */
    H[2] = SCM_VECTOR4F_DOTV(r[1], r[2]);
    r[2][0] -= H[2]*r[1][0];
    r[2][1] -= H[2]*r[1][1];
    r[2][2] -= H[2]*r[1][2];
    
    /* Scale Z */
    S[2] = SCM_VECTOR4F_NORMV(r[2]);
    if (S[2] != 0.0) {
        SCM_VECTOR4F_OP(_, r[2][_] /= S[2]); /* normalize */
        H[1] /= S[2];
        H[2] /= S[2];
    }
    
    S[3] = H[3] = 0.0;

    /* Adjust if flipped */
    SCM_VECTOR4F_CROSSV(temp, r[1], r[2]);
    det = SCM_VECTOR4F_DOTV(r[0], temp);
    if (det < 0.0) {
        for (i=0; i<3; i++) {
            S[i] = -S[i];
            r[i][0] = -r[i][0];
            r[i][1] = -r[i][1];
            r[i][2] = -r[i][2];
        }
    }
    if (r[0][2] < -1.0) r[0][2] = -1.0;
    if (r[0][2] >  1.0) r[0][2] =  1.0;

    /* Store rotation matrix */
    for (i=0; i<3; i++) {
        R[i*4]   = r[i][0];
        R[i*4+1] = r[i][1];
        R[i*4+2] = r[i][2];
        R[i*4+3] = 0.0;
    }
    R[12] = R[13] = R[14] = 0.0; R[15] = 1.0;

    if (S[0] == 0.0 || S[1] == 0.0 || S[2] == 0.0) return FALSE;
    else return TRUE;
}

/*
 * Recover rotation from orthogonal matrix.
 */
float Scm_Matrix4fToRotationv(const float m[], float v[])
{
    float q[4];
    float theta, sint;
    
    Scm_Matrix4fToQuatfv(q, m);
    theta = atan2f(sqrtf(q[0]*q[0]+q[1]*q[1]+q[2]*q[2]), q[3]);
    sint = sinf(theta);
    if (fabs(sint) < 1.0e-6) {
        v[0] = v[1] = v[2] = v[3] = 0.0;
        return 0.0;
    } else {
        v[0] = q[0]/sint;
        v[1] = q[1]/sint;
        v[2] = q[2]/sint;
        v[3] = 0.0;
        return theta*2;
    }
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

ScmObj Scm_QuatfSetv(ScmQuatf *q, const float d[])
{
    float *fv = SCM_QUATF_D(q);
    fv[0]=d[0]; fv[1]=d[1]; fv[2]=d[2]; fv[3]=d[3];
    SCM_RETURN(SCM_OBJ(q));
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

/*
 * Quaternion <-> Matrix
 */
void Scm_QuatfToMatrix4fv(float m[], const float q[])
{
    float x2 = q[0]*q[0], y2 = q[1]*q[1], z2 = q[2]*q[2];
    float xy = q[0]*q[1], yz = q[1]*q[2], zx = q[2]*q[0];
    float xw = q[0]*q[3], yw = q[1]*q[3], zw = q[2]*q[3];
    m[0] = 1-2*(y2+z2); m[4] = 2*(xy-zw); m[8] = 2*(zx+yw); m[12] = 0;
    m[1] = 2*(xy+zw); m[5] = 1-2*(z2+x2); m[9] = 2*(yz-xw); m[13] = 0;
    m[2] = 2*(zx-yw); m[6] = 2*(yz+xw); m[10] = 1-2*(x2+y2); m[14] = 0;
    m[3] = 0; m[7] = 0; m[11] = 0; m[15] = 1;
}

void Scm_Matrix4fToQuatfv(float q[], const float m[])
{
    float trace = m[0] + m[5] + m[10];
    float s;
    if (trace > 0.0) {
        s = 0.5 / sqrtf(trace + 1.0);
        q[0] = (m[6]-m[9])*s;
        q[1] = (m[8]-m[2])*s;
        q[2] = (m[1]-m[4])*s;
        q[3] = 0.25 / s;
    } else {
        static int next[] = { 1, 2, 0 }; /* to avoid modulo */
        int i, j, k;
        /* find max elem in m(i,i) for i=0 to 2. */
        if (m[0] >= m[5]) {
            if (m[0] >= m[10]) i = 0;
            else i = 2;
        } else {
            if (m[5] >= m[10]) i = 1;
            else i = 2;
        }
        j = next[i]; k = next[j];
        s = sqrtf((m[i*5] - (m[j*5]+m[k*5])) + 1.0);
        if (m[j*4+k] < m[k*4+j]) s *= -1.0;
        q[i] = 0.5 * s;
        s = 0.5 / s;
        q[j] = (m[j*4+i] + m[i*4+j]) * s;
        q[k] = (m[k*4+i] + m[i*4+k]) * s;
        q[3] = (m[j*4+k] - m[k*4+j]) * s;
    }
}

/*
 * Interpolation
 */
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

/*
 * Conversion between Euler angles and Quartenions
 */



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

/*
 Appendix: a script to generate Euler->matrix4f calculation code.
           See the definition of Scm_EulerToMatrix4fv above.

 (use srfi-1)
 (define (mref m i j) (list-ref m (+ (* j 3) i)))
 (define (tmul t0 t1)
   (cond ((or (eqv? t0 0) (eqv? t1 0)) 0)
         ((eqv? t0 1) t1)
         ((eqv? t1 1) t0)
         ((eq? (car t0) '-)
          (if (eq? (car t1) '-)
              (append (cdr t0) (cdr t1))
              (cons '- (append (cdr t0) t1))))
         (else
          (if (eq? (car t1) '-)
              (cons '- (append t0 (cdr t1)))
              (append t0 t1)))))
 (define (mmul m n)
   (let ((r '()))
     (dotimes (j 3)
       (dotimes (i 3)
         (let1 term (delete 0 (map (lambda (k) (tmul (mref m i k) (mref n k j)))
                                   '(0 1 2)))
           (push! r (cond ((null? term) 0)
                          ((null? (cdr term)) (car term))
                          (else (list (cons '+ term))))))))
     (reverse r)))
 (define (mmul3 m n o) (mmul m (mmul n o)))
 (define (fmt m)
   (define (fmt-e e)
     (cond ((not (pair? e)) (x->string e))
           ((eq? (car e) '-)
            (string-append "-" (string-join (map x->string (cdr e)) "*")))
           ((symbol? (car e))
            (string-join (map x->string e) "*"))
           (else
            (string-join (map fmt-e (cdar e)) " + "))))
   (let ((elts (map fmt-e m))
         (ind  '(0 1 2 4 5 6 8 9 10)))
     (dotimes (i 9)
       (print #`"        m[,(list-ref ind i)] = ,(list-ref elts i);")))
   )

 (define Rx '(1 0 0 0 (cx) (sx) 0 (- sx) (cx)))
 (define Ry '((cy) 0 (- sy) 0 1 0 (sy) 0 (cy)))
 (define Rz '((cz) (sz) 0 (- sz) (cz) 0 0 0 1))
 (define R `((X ,Rx) (Y ,Ry) (Z ,Rz)))

 (define (pcase order)
   (apply format #t "    case SCM_MATH3D_ROTATE_~a~a~a:\n" order)
   (fmt (apply mmul3 (reverse (map (lambda (key) (cond ((assoc key R) => cadr)))
                                   order))))
   (print "        break;"))

 (pcase '(X Y Z))
 (pcase '(X Z Y))
 (pcase '(Y Z X))
 (pcase '(Y X Z))
 (pcase '(Z X Y))
 (pcase '(Z Y X))
*/
