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
 *  $Id: gauche-math3d.c,v 1.3 2002-09-27 10:48:46 shirok Exp $
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

SCM_DEFINE_BUILTIN_CLASS(Scm_Vec4Class, vec4_print, vec4_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakeVec4(float x, float y, float z, float w)
{
    ScmVec4 *v = SCM_NEW(ScmVec4);
    SCM_SET_CLASS(v, SCM_CLASS_VEC4);
    v->v = ALLOC_FV(4);
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
    v->v = SCM_F32VECTOR_ELEMENTS(fv); /* share the vector */
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

ScmObj Scm_Vec4ToList(const ScmVec4 *v)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_VEC4_D(v)[0]),
                     Scm_MakeFlonum(SCM_VEC4_D(v)[1]),
                     Scm_MakeFlonum(SCM_VEC4_D(v)[2]),
                     Scm_MakeFlonum(SCM_VEC4_D(v)[3]));
}

float Scm_Vec4Dot(const ScmVec4 *p, const ScmVec4 *q)
{
    return (SCM_VEC4_DOTV(SCM_VEC4_D(p), SCM_VEC4_D(q)));
}

float Scm_Vec4Dotv(const float *p, const float *q)
{
    return SCM_VEC4_DOTV(p, q);
}

ScmObj Scm_Vec4Cross(const ScmVec4 *p, const ScmVec4 *q)
{
    float r[4];
    SCM_VEC4_CROSSV(r, SCM_VEC4_D(p), SCM_VEC4_D(q));
    return Scm_MakeVec4v(r);
}

void Scm_Vec4Crossv(float *r, const float *p, const float *q)
{
    SCM_VEC4_CROSSV(r, p, q);
}

ScmObj Scm_Vec4Normalize(const ScmVec4 *p)
{
    float r[4];
    r[0] = SCM_VEC4_D(p)[0];
    r[1] = SCM_VEC4_D(p)[1];
    r[2] = SCM_VEC4_D(p)[2];
    r[3] = SCM_VEC4_D(p)[3];
    SCM_VEC4_NORMALIZEV(r);
    return Scm_MakeVec4v(r);
}

void Scm_Vec4Normalizev(float *p)
{
    SCM_VEC4_NORMALIZEV(p);
}

ScmObj Scm_Vec4NormalizeX(ScmVec4 *p)
{
    SCM_VEC4_NORMALIZEV(SCM_VEC4_D(p));
    return SCM_OBJ(p);
}

ScmObj Scm_Vec4Add(const ScmVec4 *p, const ScmVec4 *q)
{
    float r[4];
    SCM_VEC4_ADDV(r, SCM_VEC4_D(p), SCM_VEC4_D(q));
    return Scm_MakeVec4v(r);
}

void Scm_Vec4Addv(float *r, const float *p, const float *q)
{
    SCM_VEC4_ADDV(r, p, q);
}

ScmObj Scm_Vec4Sub(const ScmVec4 *p, const ScmVec4 *q)
{
    float r[4];
    SCM_VEC4_SUBV(r, SCM_VEC4_D(p), SCM_VEC4_D(q));
    return Scm_MakeVec4v(r);
}

void Scm_Vec4Subv(float *r, const float *p, const float *q)
{
    SCM_VEC4_SUBV(r, p, q);
}

/*=============================================================
 * VectorArray
 */

static void vec4_array_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    ScmVec4Array *va = SCM_VEC4_ARRAY(obj);
    int len = SCM_VEC4_ARRAY_SIZE(va), i;
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
        int i, len = SCM_VEC4_ARRAY_SIZE(x);
        float *p = SCM_VEC4_ARRAY_D(x), *q = SCM_VEC4_ARRAY_D(y);
        if (len != SCM_VEC4_ARRAY_SIZE(y)) return 0;
        for (i=0; i<len*4; i++) {
            if (*p++ != *q++) return 0;
        }
        return -1;
    } else {
        Scm_Error("can't order %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_Vec4ArrayClass,
                         vec4_array_print, vec4_array_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakeVec4Arrayv(int nvecs, const float *init)
{
    int i, j;
    ScmVec4Array *a;
    SCM_ASSERT(nvecs >= 0);
    a = SCM_NEW(ScmVec4Array);
    SCM_SET_CLASS(a, SCM_CLASS_VEC4_ARRAY);
    a->size = nvecs;
    a->v = ALLOC_FV(nvecs*4);
    if (init) {
        for (i=0;i<nvecs;i++) {
            SCM_VEC4_ARRAY_SET(a, i, init[0], init[1], init[2], init[3]);
        }
    } else {
        for (i=0;i<nvecs*4;i++) SCM_VEC4_ARRAY_D(a)[i] = 0.0;
    }
    return SCM_OBJ(a);
}

ScmObj Scm_MakeVec4ArrayV(ScmF32Vector *fv)
{
    int size;
    ScmVec4Array *a;
    size = SCM_F32VECTOR_SIZE(fv);
    if (size % 4 != 0) {
        Scm_Error("f32vector size must be multiple of 4, but got %S", fv);
    }
    a = SCM_NEW(ScmVec4Array);
    SCM_SET_CLASS(a, SCM_CLASS_VEC4_ARRAY);
    a->size = size/4;
    a->v = SCM_F32VECTOR_ELEMENTS(fv); /* share the storage */
    return SCM_OBJ(a);
}

ScmObj Scm_Vec4ArrayRef(const ScmVec4Array *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_VEC4_ARRAY_SIZE(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_MakeVec4v(SCM_VEC4_ARRAY_REFV(a, n));
}

float *Scm_Vec4ArrayRefv(ScmVec4Array *a, int n)
{
    return SCM_VEC4_ARRAY_REFV(a, n);
}

void Scm_Vec4ArraySet(ScmVec4Array *a, int n, ScmVec4 *v)
{
    Scm_Vec4ArraySetv(a, n, SCM_VEC4_D(v));
}

void Scm_Vec4ArraySetv(ScmVec4Array *a, int n, float d[])
{
    if (n < 0 || n >= SCM_VEC4_ARRAY_SIZE(a)) {
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
    ScmPoint4 *v = SCM_NEW(ScmPoint4);
    SCM_SET_CLASS(v, SCM_CLASS_POINT4);
    v->v = ALLOC_FV(4);
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
    v->v = SCM_F32VECTOR_ELEMENTS(fv);
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
    if (SCM_POINT4P(q)) {
        SCM_VEC4_SUBV(r, SCM_POINT4_D(p), SCM_POINT4_D(q));
        return Scm_MakeVec4v(r);
    }
    if (SCM_VEC4P(q)) {
        SCM_VEC4_SUBV(r, SCM_POINT4_D(p), SCM_VEC4_D(q));
        return Scm_MakePoint4v(r);
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
    int len = SCM_POINT4_ARRAY_SIZE(va), i;
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
    SCM_SET_CLASS(a, SCM_CLASS_POINT4_ARRAY);
    a->size = len;
    a->v = ALLOC_FV(len*4);
    if (init) {
        for (i=0;i<len;i++) {
            SCM_POINT4_ARRAY_SET(a, i, init[0], init[1], init[2], init[3]);
        }
    } else {
        for (i=0;i<len*4;i++) SCM_POINT4_ARRAY_D(a)[i] = 0.0;
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
    a->size = size/4;
    a->v = SCM_F32VECTOR_ELEMENTS(fv);
    return SCM_OBJ(a);
}

ScmObj Scm_Point4ArrayRef(const ScmPoint4Array *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_POINT4_ARRAY_SIZE(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_MakePoint4v(SCM_POINT4_ARRAY_REFV(a, n));
}

float *Scm_Point4ArrayRefv(ScmPoint4Array *a, int n)
{
    return SCM_POINT4_ARRAY_REFV(a, n);
}

void Scm_Point4ArraySet(ScmPoint4Array *a, int n, ScmPoint4 *v)
{
    if (n < 0 || n >= SCM_POINT4_ARRAY_SIZE(a)) {
        Scm_Error("index out of range");
    }
    SCM_POINT4_ARRAY_SET(a, n,
                         SCM_POINT4_D(v)[0],
                         SCM_POINT4_D(v)[1],
                         SCM_POINT4_D(v)[2],
                         SCM_POINT4_D(v)[3]);
}

void Scm_Point4ArraySetv(ScmPoint4Array *a, int n, float d[])
{
    SCM_POINT4_ARRAY_SET(a, n, d[0], d[1], d[2], d[3]);
}

/*=============================================================
 * Matrix
 */

static void mat4_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    int i;
    ScmMat4 *m = SCM_MAT4(obj);
    Scm_Printf(out, "#,(mat4");
    for (i=0; i<16; i++) {
        Scm_Printf(out, " %g", SCM_MAT4_D(m)[i]);
    }
    Scm_Printf(out, ")");
}

static int mat4_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        int i;
        float *p = SCM_MAT4_D(x), *q = SCM_MAT4_D(y);
        for (i=0; i<16; i++) {
            if (*p != *q) return -1;
        }
        return 0;
    } else {
        Scm_Error("can't order matrix %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_Mat4Class, mat4_print, mat4_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_MakeMat4v(const float d[])
{
    ScmMat4 *m = SCM_NEW(ScmMat4);
    int i;
    SCM_SET_CLASS(m, SCM_CLASS_MAT4);
    m->v = ALLOC_FV(16);
    if (d == NULL) {
        for (i=0; i<16; i++) SCM_MAT4_D(m)[i] = ((i/4==i%4)? 1.0 : 0.0);
    } else {
        for (i=0; i<16; i++) SCM_MAT4_D(m)[i] = d[i];
    }
    return SCM_OBJ(m);
}

ScmObj Scm_ListToMat4(ScmObj l)
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
    return Scm_MakeMat4v(d);
  badlist:
    Scm_Error("list of 16 real numbers required, but got %S", l);
    return SCM_UNDEFINED;       /* dummy */
}

/* Matrix X Matrix */
void Scm_Mat4MulMat4v(float *r, const float *p, const float *q)
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

ScmObj Scm_MatrixMulMatrix(const ScmMat4 *p, const ScmMat4 *q)
{
    ScmMat4 *r = SCM_MAT4(Scm_MakeMat4v(NULL));
    Scm_Mat4MulMat4v(SCM_MAT4_D(r), SCM_MAT4_D(p), SCM_MAT4_D(q));
    return SCM_OBJ(r);
}

/* Matrix X Vector */
void Scm_Mat4MulVec4v(float *r, const float *m, const float *v)
{
    r[0] = m[0]*v[0]+m[4]*v[1]+m[8]*v[2]+m[12]*v[3];
    r[1] = m[1]*v[0]+m[5]*v[1]+m[9]*v[2]+m[13]*v[3];
    r[2] = m[2]*v[0]+m[6]*v[1]+m[10]*v[2]+m[14]*v[3];
    r[3] = m[3]*v[0]+m[7]*v[1]+m[11]*v[2]+m[15]*v[3];
}

ScmObj Scm_Mat4MulVec4(const ScmMat4 *m, const ScmVec4 *v)
{
    ScmVec4 *r = SCM_VEC4(Scm_MakeVec4v(NULL));
    Scm_Mat4MulVec4v(SCM_MAT4_D(r), SCM_MAT4_D(m), SCM_MAT4_D(v));
    return SCM_OBJ(r);
}

ScmObj Scm_Mat4MulPoint4(const ScmMat4 *m, const ScmPoint4 *p)
{
    ScmPoint4 *r = SCM_POINT4(Scm_MakePoint4v(NULL));
    Scm_Mat4MulVec4v(SCM_MAT4_D(r), SCM_MAT4_D(m), SCM_MAT4_D(p));
    return SCM_OBJ(r);
}

void   Scm_Mat4Scalev(float *r, double f)
{
    int i;
    for (i=0; i<16; i++) r[i] *= f;
}

ScmObj Scm_Mat4Scale(const ScmMat4 *m, double f)
{
    ScmMat4 *r = SCM_MAT4(Scm_MakeMat4v(NULL));
    Scm_Mat4Scalev(SCM_MAT4_D(r), f);
    return SCM_OBJ(r);
}

/*=============================================================
 * Quaternion
 */
static void quat_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(quat %g %g %g %g)",
               SCM_QUAT_D(obj)[0],
               SCM_QUAT_D(obj)[1],
               SCM_QUAT_D(obj)[2],
               SCM_QUAT_D(obj)[3]);
}

static int quat_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        if (SCM_QUAT_D(x)[0] == SCM_QUAT_D(y)[0]
            && SCM_QUAT_D(x)[1] == SCM_QUAT_D(y)[1]
            && SCM_QUAT_D(x)[2] == SCM_QUAT_D(y)[2]
            && SCM_QUAT_D(x)[3] == SCM_QUAT_D(y)[3]) {
            return 0;
        } else {
            return -1;
        }
    } else {
        Scm_Error("can't order quat %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_QuatClass, quat_print, quat_compare,
                         NULL, NULL, sequenceCPL);

/*
 * Constructors and converters
 */
ScmObj Scm_MakeQuat(float x, float y, float z, float w)
{
    ScmQuat *v = SCM_NEW(ScmQuat);
    SCM_SET_CLASS(v, SCM_CLASS_QUAT);
    SCM_QUAT_D(v) = ALLOC_FV(4);
    SCM_QUAT_D(v)[0] = x;
    SCM_QUAT_D(v)[1] = y;
    SCM_QUAT_D(v)[2] = z;
    SCM_QUAT_D(v)[3] = w;
    return SCM_OBJ(v);
}

ScmObj Scm_MakeQuatv(const float d[])
{
    if (d) return Scm_MakeQuat(d[0], d[1], d[2], d[3]);
    else   return Scm_MakeQuat(0.0, 0.0, 0.0, 1.0);
}

ScmObj Scm_MakeQuatV(ScmF32Vector *fv)
{
    ScmQuat *v = SCM_NEW(ScmQuat);
    SCM_SET_CLASS(v, SCM_CLASS_QUAT);
    SCM_QUAT_D(v) = SCM_F32VECTOR_ELEMENTS(fv); /* share storage */
    return SCM_OBJ(v);
}

ScmObj Scm_ListToQuat(ScmObj l)
{
    int i;
    float d[4];
    ScmObj lp = l;
    for (i=0; i<4; i++, lp = SCM_CDR(lp)) {
        if (!SCM_PAIRP(lp)) goto badlist;
        if (!SCM_REALP(SCM_CAR(lp))) goto badlist;
        d[i] = (float)Scm_GetDouble(SCM_CAR(lp));
    }
    return Scm_MakeQuatv(d);
  badlist:
    Scm_Error("list of 3 or 4 real numbers required, but got %S", l);
    return SCM_UNDEFINED;
}

ScmObj Scm_QuatToList(const ScmQuat *p)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_QUAT_D(p)[0]),
                     Scm_MakeFlonum(SCM_QUAT_D(p)[1]),
                     Scm_MakeFlonum(SCM_QUAT_D(p)[2]),
                     Scm_MakeFlonum(SCM_QUAT_D(p)[3]));
}

void Scm_QuatAddv(float *r, const float *p, const float *q)
{
    SCM_VEC4_OP(i, r[i] = p[i] + q[i]);
}

ScmObj Scm_QuatAdd(const ScmQuat *p, const ScmQuat *q)
{
    float r[4];
    SCM_VEC4_OP(i, r[i] = SCM_QUAT_D(p)[i] + SCM_QUAT_D(q)[i]);
    return Scm_MakeQuatv(r);
}

void Scm_QuatSubv(float *r, const float *p, const float *q)
{
    SCM_VEC4_OP(i, r[i] = p[i] - q[i]);
}

ScmObj Scm_QuatSub(const ScmQuat *p, const ScmQuat *q)
{
    float r[4];
    SCM_VEC4_OP(i, r[i] = SCM_QUAT_D(p)[i] - SCM_QUAT_D(q)[i]);
    return Scm_MakeQuatv(r);
}

void Scm_QuatMulv(float *r, const float *p, const float *q)
{
    r[0] = p[0]*q[3]+p[1]*q[2]-p[2]*q[1]+p[3]*q[0];
    r[1] = p[1]*q[3]+p[2]*q[0]-p[0]*q[2]+p[3]*q[1];
    r[2] = p[2]*q[3]+p[0]*q[1]-p[1]*q[0]+p[3]*q[2];
    r[3] = p[0]*q[0]+p[1]*q[1]+p[2]*q[2]+p[3]*q[3];
}

ScmObj Scm_QuatMul(const ScmQuat *p, const ScmQuat *q)
{
    float r[4];
    Scm_QuatMulv(r, SCM_QUAT_D(p), SCM_QUAT_D(q));
    return Scm_MakeQuatv(r);
}

ScmObj Scm_QuatNormalize(const ScmQuat *p)
{
    float r[4];
    r[0] = SCM_QUAT_D(p)[0];
    r[1] = SCM_QUAT_D(p)[1];
    r[2] = SCM_QUAT_D(p)[2];
    r[3] = SCM_QUAT_D(p)[3];
    SCM_QUAT_NORMALIZEV(r);
    return Scm_MakeQuatv(r);
}

ScmObj Scm_QuatNormalizeX(ScmQuat *p)
{
    SCM_QUAT_NORMALIZEV(SCM_QUAT_D(p));
    return SCM_OBJ(p);
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
    Scm_InitBuiltinClass(&Scm_Vec4Class, "<vec4>",
                         NULL, sizeof(ScmVec4)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_Vec4ArrayClass, "<vec4-array>",
                         NULL, sizeof(ScmVec4Array)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_Point4Class, "<point4>",
                         NULL, sizeof(ScmPoint4)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_Point4ArrayClass, "<point4-array>",
                         NULL, sizeof(ScmPoint4Array)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_Mat4Class, "<mat4>",
                         NULL, sizeof(ScmMat4)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_QuatClass, "<quat>",
                         NULL, sizeof(ScmQuat)/sizeof(ScmObj),
                         mod);
/*    Scm_Init_math3d_lib(mod);*/
}

