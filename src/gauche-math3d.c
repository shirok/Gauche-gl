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
 *  $Id: gauche-math3d.c,v 1.5 2002-09-27 21:51:58 shirok Exp $
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
 * 3DVector
 */
static void vec_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(<3dvector> %gf %g %g %g)",
               SCM_3DVECTOR_D(obj)[0],
               SCM_3DVECTOR_D(obj)[1],
               SCM_3DVECTOR_D(obj)[2],
               SCM_3DVECTOR_D(obj)[3]);
}

static int vec_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        if (SCM_3DVECTOR_D(x)[0] == SCM_3DVECTOR_D(y)[0]
            && SCM_3DVECTOR_D(x)[1] == SCM_3DVECTOR_D(y)[1]
            && SCM_3DVECTOR_D(x)[2] == SCM_3DVECTOR_D(y)[2]
            && SCM_3DVECTOR_D(x)[3] == SCM_3DVECTOR_D(y)[3]) {
            return 0;
        } else {
            return -1;
        }
    } else {
        Scm_Error("can't order %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_3DVectorClass, vec_print, vec_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_Make3DVector(float x, float y, float z, float w)
{
    Scm3DVector *v = SCM_NEW(Scm3DVector);
    SCM_SET_CLASS(v, SCM_CLASS_3DVECTOR);
    v->v = ALLOC_FV(4);
    SCM_3DVECTOR_D(v)[0] = x; SCM_3DVECTOR_D(v)[1] = y;
    SCM_3DVECTOR_D(v)[2] = z; SCM_3DVECTOR_D(v)[3] = w;
    return SCM_OBJ(v);
}

ScmObj Scm_Make3DVectorV(ScmF32Vector *fv)
{
    Scm3DVector *v;
    CHECK_F32V(fv, 4);
    v = SCM_NEW(Scm3DVector);
    SCM_SET_CLASS(v, SCM_CLASS_3DVECTOR);
    v->v = SCM_F32VECTOR_ELEMENTS(fv); /* share the vector */
    return SCM_OBJ(v);
}

ScmObj Scm_Make3DVectorv(const float *d)
{
    if (d) return Scm_Make3DVector(d[0], d[1], d[2], d[3]);
    else   return Scm_Make3DVector(0.0, 0.0, 0.0, 0.0);
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
        d[3] = init3;
    }
    if (SCM_NULLP(lp)) return;
  badlist:
    Scm_Error("list of 3 or 4 real numbers required, but got %S", l);
}

ScmObj Scm_ListTo3DVector(ScmObj l)
{
    float d[4];
    list2vec(l, d, 0.0);
    return Scm_Make3DVectorv(d);
}

ScmObj Scm_3DVectorToList(const Scm3DVector *v)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_3DVECTOR_D(v)[0]),
                     Scm_MakeFlonum(SCM_3DVECTOR_D(v)[1]),
                     Scm_MakeFlonum(SCM_3DVECTOR_D(v)[2]),
                     Scm_MakeFlonum(SCM_3DVECTOR_D(v)[3]));
}

float Scm_3DVectorDot(const Scm3DVector *p, const Scm3DVector *q)
{
    return (SCM_3DVECTOR_DOTV(SCM_3DVECTOR_D(p), SCM_3DVECTOR_D(q)));
}

float Scm_3DVectorDotv(const float *p, const float *q)
{
    return SCM_3DVECTOR_DOTV(p, q);
}

ScmObj Scm_3DVectorCross(const Scm3DVector *p, const Scm3DVector *q)
{
    float r[4];
    SCM_3DVECTOR_CROSSV(r, SCM_3DVECTOR_D(p), SCM_3DVECTOR_D(q));
    return Scm_Make3DVectorv(r);
}

void Scm_3DVectorCrossv(float *r, const float *p, const float *q)
{
    SCM_3DVECTOR_CROSSV(r, p, q);
}

ScmObj Scm_3DVectorNormalize(const Scm3DVector *p)
{
    float r[4];
    r[0] = SCM_3DVECTOR_D(p)[0];
    r[1] = SCM_3DVECTOR_D(p)[1];
    r[2] = SCM_3DVECTOR_D(p)[2];
    r[3] = SCM_3DVECTOR_D(p)[3];
    SCM_3DVECTOR_NORMALIZEV(r);
    return Scm_Make3DVectorv(r);
}

void Scm_3DVectorNormalizev(float *p)
{
    SCM_3DVECTOR_NORMALIZEV(p);
}

ScmObj Scm_3DVectorNormalizeX(Scm3DVector *p)
{
    SCM_3DVECTOR_NORMALIZEV(SCM_3DVECTOR_D(p));
    return SCM_OBJ(p);
}

ScmObj Scm_3DVectorAdd(const Scm3DVector *p, const Scm3DVector *q)
{
    float r[4];
    SCM_3DVECTOR_ADDV(r, SCM_3DVECTOR_D(p), SCM_3DVECTOR_D(q));
    return Scm_Make3DVectorv(r);
}

void Scm_3DVectorAddv(float *r, const float *p, const float *q)
{
    SCM_3DVECTOR_ADDV(r, p, q);
}

ScmObj Scm_3DVectorSub(const Scm3DVector *p, const Scm3DVector *q)
{
    float r[4];
    SCM_3DVECTOR_SUBV(r, SCM_3DVECTOR_D(p), SCM_3DVECTOR_D(q));
    return Scm_Make3DVectorv(r);
}

void Scm_3DVectorSubv(float *r, const float *p, const float *q)
{
    SCM_3DVECTOR_SUBV(r, p, q);
}

/*=============================================================
 * VectorArray
 */

static void vec_array_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm3DVectorArray *va = SCM_3DVECTOR_ARRAY(obj);
    int len = SCM_3DVECTOR_ARRAY_SIZE(va), i;
    Scm_Printf(out, "#,(<3dvecor-array> %d ", len);
    for (i = 0; i < len; i++) {
        float *z = Scm_3DVectorArrayRefv(va, i);
        Scm_Printf(out, "(%g %g %g %g) ", z[0], z[1], z[2], z[3]);
    }
    Scm_Printf(out, ")");
}

static int vec_array_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        int i, len = SCM_3DVECTOR_ARRAY_SIZE(x);
        float *p = SCM_3DVECTOR_ARRAY_D(x), *q = SCM_3DVECTOR_ARRAY_D(y);
        if (len != SCM_3DVECTOR_ARRAY_SIZE(y)) return 0;
        for (i=0; i<len*4; i++) {
            if (*p++ != *q++) return 0;
        }
        return -1;
    } else {
        Scm_Error("can't order %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_3DVectorArrayClass,
                         vec_array_print, vec_array_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_Make3DVectorArrayv(int nvecs, const float *init)
{
    int i, j;
    Scm3DVectorArray *a;
    SCM_ASSERT(nvecs >= 0);
    a = SCM_NEW(Scm3DVectorArray);
    SCM_SET_CLASS(a, SCM_CLASS_3DVECTOR_ARRAY);
    a->size = nvecs;
    a->v = ALLOC_FV(nvecs*4);
    if (init) {
        for (i=0;i<nvecs;i++) {
            SCM_3DVECTOR_ARRAY_SET(a, i, init[0], init[1], init[2], init[3]);
        }
    } else {
        for (i=0;i<nvecs*4;i++) SCM_3DVECTOR_ARRAY_D(a)[i] = 0.0;
    }
    return SCM_OBJ(a);
}

ScmObj Scm_Make3DVectorArrayV(ScmF32Vector *fv)
{
    int size;
    Scm3DVectorArray *a;
    size = SCM_F32VECTOR_SIZE(fv);
    if (size % 4 != 0) {
        Scm_Error("f32vector size must be multiple of 4, but got %S", fv);
    }
    a = SCM_NEW(Scm3DVectorArray);
    SCM_SET_CLASS(a, SCM_CLASS_3DVECTOR_ARRAY);
    a->size = size/4;
    a->v = SCM_F32VECTOR_ELEMENTS(fv); /* share the storage */
    return SCM_OBJ(a);
}

ScmObj Scm_3DVectorArrayRef(const Scm3DVectorArray *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_3DVECTOR_ARRAY_SIZE(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_Make3DVectorv(SCM_3DVECTOR_ARRAY_REFV(a, n));
}

float *Scm_3DVectorArrayRefv(Scm3DVectorArray *a, int n)
{
    return SCM_3DVECTOR_ARRAY_REFV(a, n);
}

void Scm_3DVectorArraySet(Scm3DVectorArray *a, int n, Scm3DVector *v)
{
    Scm_3DVectorArraySetv(a, n, SCM_3DVECTOR_D(v));
}

void Scm_3DVectorArraySetv(Scm3DVectorArray *a, int n, float d[])
{
    if (n < 0 || n >= SCM_3DVECTOR_ARRAY_SIZE(a)) {
        Scm_Error("index out of range");
    }
    SCM_3DVECTOR_ARRAY_SET(a, n, d[0], d[1], d[2], d[3]);
}

/*=============================================================
 * Pointf - is just a vector with different default value.
 */
static void point_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(<3dpoint> %g %g %g %g)",
               SCM_3DVECTOR_D(obj)[0],
               SCM_3DVECTOR_D(obj)[1],
               SCM_3DVECTOR_D(obj)[2],
               SCM_3DVECTOR_D(obj)[3]);
}

#define point_compare vec_compare

SCM_DEFINE_BUILTIN_CLASS(Scm_3DPointClass, point_print, point_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_Make3DPoint(float x, float y, float z, float w)
{
    Scm3DPoint *v = SCM_NEW(Scm3DPoint);
    SCM_SET_CLASS(v, SCM_CLASS_3DPOINT);
    v->v = ALLOC_FV(4);
    SCM_3DPOINT_D(v)[0] = x; SCM_3DPOINT_D(v)[1] = y;
    SCM_3DPOINT_D(v)[2] = z; SCM_3DPOINT_D(v)[3] = w;
    return SCM_OBJ(v);
}

ScmObj Scm_Make3DPointv(const float *d)
{
    if (d) return Scm_Make3DPoint(d[0], d[1], d[2], d[3]);
    else   return Scm_Make3DPoint(0.0, 0.0, 0.0, 0.0);
}

ScmObj Scm_Make3DPointV(ScmF32Vector *fv)
{
    Scm3DPoint *v;
    CHECK_F32V(fv, 4);
    v = SCM_NEW(Scm3DPoint);
    SCM_SET_CLASS(v, SCM_CLASS_3DPOINT);
    v->v = SCM_F32VECTOR_ELEMENTS(fv);
    return SCM_OBJ(v);
}

ScmObj Scm_ListTo3DPoint(ScmObj l)
{
    float d[4];
    list2vec(l, d, 1.0);
    return Scm_Make3DPointv(d);
}

ScmObj Scm_3DPointToList(const Scm3DPoint *p)
{
    return SCM_LIST4(Scm_MakeFlonum(SCM_3DPOINT_D(p)[0]),
                     Scm_MakeFlonum(SCM_3DPOINT_D(p)[1]),
                     Scm_MakeFlonum(SCM_3DPOINT_D(p)[2]),
                     Scm_MakeFlonum(SCM_3DPOINT_D(p)[3]));
}

ScmObj Scm_3DPointAdd(const Scm3DPoint *p, const Scm3DVector *q)
{
    float r[4];
    SCM_3DVECTOR_ADDV(r, SCM_3DPOINT_D(p), SCM_3DVECTOR_D(q));
    return Scm_Make3DPointv(r);
}

ScmObj Scm_3DPointSub(const Scm3DVector *p, const ScmObj q)
{
    float r[4];
    if (SCM_3DPOINTP(q)) {
        SCM_3DVECTOR_SUBV(r, SCM_3DPOINT_D(p), SCM_3DPOINT_D(q));
        return Scm_Make3DVectorv(r);
    }
    if (SCM_3DVECTORP(q)) {
        SCM_3DVECTOR_SUBV(r, SCM_3DPOINT_D(p), SCM_3DVECTOR_D(q));
        return Scm_Make3DPointv(r);
    }
    Scm_Error("<3dpoint> or <3dvector> required, but got %S", q);
    return SCM_UNDEFINED;
}

/*=============================================================
 * 3DPointArray
 */

static void point_array_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm3DPointArray *va = SCM_3DPOINT_ARRAY(obj);
    int len = SCM_3DPOINT_ARRAY_SIZE(va), i;
    Scm_Printf(out, "#,(<3dpoint-array> %d ", len);
    for (i = 0; i < len; i++) {
        float *z = Scm_3DPointArrayRefv(va, i);
        Scm_Printf(out, "(%g %g %g %g) ", z[0], z[1], z[2], z[3]);
    }
    Scm_Printf(out, ")");
}

#define point_array_compare vec_array_compare

SCM_DEFINE_BUILTIN_CLASS(Scm_3DPointArrayClass, point_array_print,
                         point_array_compare, NULL, NULL, sequenceCPL);

ScmObj Scm_Make3DPointArrayv(int len, const float *init)
{
    int i, j;
    Scm3DPointArray *a;
    SCM_ASSERT(len >= 0);
    a = SCM_NEW(Scm3DPointArray);
    SCM_SET_CLASS(a, SCM_CLASS_3DPOINT_ARRAY);
    a->size = len;
    a->v = ALLOC_FV(len*4);
    if (init) {
        for (i=0;i<len;i++) {
            SCM_3DPOINT_ARRAY_SET(a, i, init[0], init[1], init[2], init[3]);
        }
    } else {
        for (i=0;i<len*4;i++) SCM_3DPOINT_ARRAY_D(a)[i] = 0.0;
    }
    return SCM_OBJ(a);
}

ScmObj Scm_Make3DPointArrayV(ScmF32Vector *fv)
{
    int size;
    Scm3DPointArray *a;
    size = SCM_F32VECTOR_SIZE(fv);
    if (size % 4 != 0) {
        Scm_Error("f32vector size must be multiple of 4, but got %S", fv);
    }
    a = SCM_NEW(Scm3DPointArray);
    SCM_SET_CLASS(a, SCM_CLASS_3DPOINT_ARRAY);
    a->size = size/4;
    a->v = SCM_F32VECTOR_ELEMENTS(fv);
    return SCM_OBJ(a);
}

ScmObj Scm_3DPointArrayRef(const Scm3DPointArray *a, int n, ScmObj fallback)
{
    if (n < 0 || n >= SCM_3DPOINT_ARRAY_SIZE(a)) {
        if (SCM_UNBOUNDP(fallback)) Scm_Error("index out of range");
        return fallback;
    }
    return Scm_Make3DPointv(SCM_3DPOINT_ARRAY_REFV(a, n));
}

float *Scm_3DPointArrayRefv(Scm3DPointArray *a, int n)
{
    return SCM_3DPOINT_ARRAY_REFV(a, n);
}

void Scm_3DPointArraySet(Scm3DPointArray *a, int n, Scm3DPoint *v)
{
    if (n < 0 || n >= SCM_3DPOINT_ARRAY_SIZE(a)) {
        Scm_Error("index out of range");
    }
    SCM_3DPOINT_ARRAY_SET(a, n,
                         SCM_3DPOINT_D(v)[0],
                         SCM_3DPOINT_D(v)[1],
                         SCM_3DPOINT_D(v)[2],
                         SCM_3DPOINT_D(v)[3]);
}

void Scm_3DPointArraySetv(Scm3DPointArray *a, int n, float d[])
{
    SCM_3DPOINT_ARRAY_SET(a, n, d[0], d[1], d[2], d[3]);
}

/*=============================================================
 * Matrix
 */

static void mat_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    int i;
    Scm3DMatrix *m = SCM_3DMATRIX(obj);
    Scm_Printf(out, "#,(<3dmatrix>");
    for (i=0; i<16; i++) {
        Scm_Printf(out, " %g", SCM_3DMATRIX_D(m)[i]);
    }
    Scm_Printf(out, ")");
}

static int mat_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        int i;
        float *p = SCM_3DMATRIX_D(x), *q = SCM_3DMATRIX_D(y);
        for (i=0; i<16; i++) {
            if (*p != *q) return -1;
        }
        return 0;
    } else {
        Scm_Error("can't order matrix %S and %S", x, y);
        return 0;               /* dummy */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_3DMatrixClass, mat_print, mat_compare,
                         NULL, NULL, sequenceCPL);

ScmObj Scm_Make3DMatrixv(const float d[])
{
    Scm3DMatrix *m = SCM_NEW(Scm3DMatrix);
    int i;
    SCM_SET_CLASS(m, SCM_CLASS_3DMATRIX);
    m->v = ALLOC_FV(16);
    if (d == NULL) {
        for (i=0; i<16; i++) SCM_3DMATRIX_D(m)[i] = ((i/4==i%4)? 1.0 : 0.0);
    } else {
        for (i=0; i<16; i++) SCM_3DMATRIX_D(m)[i] = d[i];
    }
    return SCM_OBJ(m);
}

ScmObj Scm_Make3DMatrixV(ScmF32Vector *fv)
{
    Scm3DMatrix *m;
    if (SCM_F32VECTOR_SIZE(fv) != 16) {
        Scm_Error("f32vector of size 16 required, but got %S");
    }
    m = SCM_NEW(Scm3DMatrix);
    m->v = SCM_F32VECTOR_ELEMENTS(fv);
    return SCM_OBJ(m);
}

ScmObj Scm_ListTo3DMatrix(ScmObj l)
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
    return Scm_Make3DMatrixv(d);
  badlist:
    Scm_Error("list of 16 real numbers required, but got %S", l);
    return SCM_UNDEFINED;       /* dummy */
}

/* Matrix X Matrix */
void Scm_3DMatrixMul3DMatrixv(float *r, const float *p, const float *q)
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

ScmObj Scm_3DMatrixMul3DMatrix(const Scm3DMatrix *p, const Scm3DMatrix *q)
{
    Scm3DMatrix *r = SCM_3DMATRIX(Scm_Make3DMatrixv(NULL));
    Scm_3DMatrixMul3DMatrixv(SCM_3DMATRIX_D(r), SCM_3DMATRIX_D(p), SCM_3DMATRIX_D(q));
    return SCM_OBJ(r);
}

/* Matrix X Vector */
void Scm_3DMatrixMul3DVectorv(float *r, const float *m, const float *v)
{
    r[0] = m[0]*v[0]+m[4]*v[1]+m[8]*v[2]+m[12]*v[3];
    r[1] = m[1]*v[0]+m[5]*v[1]+m[9]*v[2]+m[13]*v[3];
    r[2] = m[2]*v[0]+m[6]*v[1]+m[10]*v[2]+m[14]*v[3];
    r[3] = m[3]*v[0]+m[7]*v[1]+m[11]*v[2]+m[15]*v[3];
}

ScmObj Scm_3DMatrixMul3DVector(const Scm3DMatrix *m, const Scm3DVector *v)
{
    Scm3DVector *r = SCM_3DVECTOR(Scm_Make3DVectorv(NULL));
    Scm_3DMatrixMul3DVectorv(SCM_3DMATRIX_D(r), SCM_3DMATRIX_D(m), SCM_3DMATRIX_D(v));
    return SCM_OBJ(r);
}

ScmObj Scm_3DMatrixMul3DPoint(const Scm3DMatrix *m, const Scm3DPoint *p)
{
    Scm3DPoint *r = SCM_3DPOINT(Scm_Make3DPointv(NULL));
    Scm_3DMatrixMul3DVectorv(SCM_3DMATRIX_D(r), SCM_3DMATRIX_D(m), SCM_3DMATRIX_D(p));
    return SCM_OBJ(r);
}

void   Scm_3DMatrixScalev(float *r, double f)
{
    int i;
    for (i=0; i<16; i++) r[i] *= f;
}

ScmObj Scm_3DMatrixScale(const Scm3DMatrix *m, double f)
{
    Scm3DMatrix *r = SCM_3DMATRIX(Scm_Make3DMatrixv(NULL));
    Scm_3DMatrixScalev(SCM_3DMATRIX_D(r), f);
    return SCM_OBJ(r);
}

/*=============================================================
 * Quaternion
 */
static void quat_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    Scm_Printf(out, "#,(<quat> %g %g %g %g)",
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
    if (SCM_F32VECTOR_SIZE(fv) != 4) {
        Scm_Error("f32vector of size 4 required, but got %S", fv);
    }
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
    SCM_3DVECTOR_OP(i, r[i] = p[i] + q[i]);
}

ScmObj Scm_QuatAdd(const ScmQuat *p, const ScmQuat *q)
{
    float r[4];
    SCM_3DVECTOR_OP(i, r[i] = SCM_QUAT_D(p)[i] + SCM_QUAT_D(q)[i]);
    return Scm_MakeQuatv(r);
}

void Scm_QuatSubv(float *r, const float *p, const float *q)
{
    SCM_3DVECTOR_OP(i, r[i] = p[i] - q[i]);
}

ScmObj Scm_QuatSub(const ScmQuat *p, const ScmQuat *q)
{
    float r[4];
    SCM_3DVECTOR_OP(i, r[i] = SCM_QUAT_D(p)[i] - SCM_QUAT_D(q)[i]);
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
    Scm_InitBuiltinClass(&Scm_3DVectorClass, "<3dvector>",
                         NULL, sizeof(Scm3DVector)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_3DVectorArrayClass, "<3dvector-array>",
                         NULL, sizeof(Scm3DVectorArray)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_3DPointClass, "<3dpoint>",
                         NULL, sizeof(Scm3DPoint)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_3DPointArrayClass, "<3dpoint-array>",
                         NULL, sizeof(Scm3DPointArray)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_3DMatrixClass, "<3dmatrix>",
                         NULL, sizeof(Scm3DMatrix)/sizeof(ScmObj),
                         mod);
    Scm_InitBuiltinClass(&Scm_QuatClass, "<quat>",
                         NULL, sizeof(ScmQuat)/sizeof(ScmObj),
                         mod);
    Scm_Init_math3d_lib(mod);
}

