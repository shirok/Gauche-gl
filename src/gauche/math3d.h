/*
 * gauche/math3d.h - 3D vector and matrix arithmetic 
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
 *  $Id: math3d.h,v 1.3 2002-09-27 10:48:50 shirok Exp $
 */

/* Vector and matrix arithmetics, specialized for 3D graphics calculation. */

#ifndef GAUCHE_MATH3D_H
#define GAUCHE_MATH3D_H

#include <gauche.h>
#include <gauche/uvector.h>

/*=============================================================
 * Common utility macros
 */

/* vector operation */
#define SCM_VEC4_OP(var, expr)                 \
    do {                                        \
       int var;                                 \
       var = 0; { expr; }                       \
       var = 1; { expr; }                       \
       var = 2; { expr; }                       \
       var = 3; { expr; }                       \
    } while (0)
       
/*=============================================================
 * 3D Vector (homogeneous coordinates)
 */

typedef struct ScmVec4Rec {
    SCM_HEADER;
    float *v;
} ScmVec4;

SCM_CLASS_DECL(Scm_Vec4Class);
#define SCM_CLASS_VEC4        (&Scm_Vec4Class)
#define SCM_VEC4P(obj)       SCM_XTYPEP(obj, SCM_CLASS_VEC4)
#define SCM_VEC4(obj)        ((ScmVec4*)(obj))
#define SCM_VEC4_D(obj)      (SCM_VEC4(obj)->v)
#define SCM_VEC4_REF(obj, i) (SCM_VEC4_D(obj)[i])

extern ScmObj Scm_MakeVec4v(const float d[]);
extern ScmObj Scm_MakeVec4(float x, float y, float z, float w);
extern ScmObj Scm_MakeVec4V(ScmF32Vector *v);
extern ScmObj Scm_ListToVec4(ScmObj l);
extern ScmObj Scm_Vec4ToList(const ScmVec4 *v);

/* SCM_VEC4_DOT(float p[4], float q[4]) */
#define SCM_VEC4_DOTV(p, q)  (p[0]*q[0]+p[1]*q[1]+p[2]*q[2]+p[3]*q[3])

/* SCM_VEC4_CROSS(float r[4], float p[4], float q[4]); r <- p x q */
#define SCM_VEC4_CROSSV(r, p, q)                \
    (r[0] = p[1]*q[2]-p[2]*q[1],                \
     r[1] = p[2]*q[0]-p[0]*q[2],                \
     r[2] = p[0]*q[1]-p[1]*q[0],                \
     r[3] = 0.0)

/* SCM_VEC4_NORMV(float p[4]) */
#define SCM_VEC4_NORMV(p)   sqrt(SCM_VEC4_DOTV(p, p))

/* SCM_VEC4_NORMALIZE(float p[4]) */
#define SCM_VEC4_NORMALIZEV(p)                  \
    do {                                        \
        float siz__ = SCM_VEC4_NORMV(p);        \
        if (siz__ != 0.0) {                     \
            SCM_VEC4_OP(i__, p[i__] /= siz__);  \
        }                                       \
    } while (0)

/* SCM_VEC4_ADD(float r[4], p[4], q[4]) */
#define SCM_VEC4_ADDV(r, p, q)                \
    SCM_VEC4_OP(i__, r[i__] = p[i__] + q[i__])

/* SCM_VEC4_SUB(float r[4], p[4], q[4]) */
#define SCM_VEC4_SUBV(r, p, q)                \
    SCM_VEC4_OP(i__, r[i__] = p[i__] - q[i__])

extern float  Scm_Vec4Dot(const ScmVec4 *p, const ScmVec4 *q);
extern float  Scm_Vec4Dotv(const float *p, const float *q);
extern ScmObj Scm_Vec4Cross(const ScmVec4 *p, const ScmVec4 *q);
extern void   Scm_Vec4Crossv(float *r, const float *p, const float *q);
extern ScmObj Scm_Vec4Normalize(const ScmVec4 *p);
extern void   Scm_Vec4Normalizev(float *p);
extern ScmObj Scm_Vec4NormalizeX(ScmVec4 *p);
extern ScmObj Scm_Vec4Add(const ScmVec4 *p, const ScmVec4 *q);
extern void   Scm_Vec4Addv(float *r, const float *p, const float *q);
extern ScmObj Scm_Vec4Sub(const ScmVec4 *p, const ScmVec4 *q);
extern void   Scm_Vec4Subv(float *r, const float *p, const float *q);

/*=============================================================
 * VectorArray
 */

typedef struct ScmVec4ArrayRec {
    SCM_HEADER;
    int size;                 /* # of vectors */
    float *v;
} ScmVec4Array;

SCM_CLASS_DECL(Scm_Vec4ArrayClass);
#define SCM_CLASS_VEC4_ARRAY       (&Scm_Vec4ArrayClass)
#define SCM_VEC4_ARRAY_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_VEC4_ARRAY)
#define SCM_VEC4_ARRAY(obj)        ((ScmVec4Array*)(obj))
#define SCM_VEC4_ARRAY_SIZE(obj)   (SCM_VEC4_ARRAY(obj)->size)
#define SCM_VEC4_ARRAY_D(obj)      (SCM_VEC4_ARRAY(obj)->v)

extern ScmObj Scm_MakeVec4Arrayv(int nvecs, const float *init);
extern ScmObj Scm_MakeVec4ArrayV(ScmF32Vector *src);

#define SCM_VEC4_ARRAY_REFV(obj, n)  (&(SCM_VEC4_ARRAY_D(obj)[(n)*4]))
#define SCM_VEC4_ARRAY_SET(obj, n, x, y, z, w)  \
   (SCM_VEC4_ARRAY_D(obj)[(n)*4] = (x),         \
    SCM_VEC4_ARRAY_D(obj)[(n)*4+1] = (y),       \
    SCM_VEC4_ARRAY_D(obj)[(n)*4+1] = (z),       \
    SCM_VEC4_ARRAY_D(obj)[(n)*4+1] = (w))

extern ScmObj Scm_Vec4ArrayRef(const ScmVec4Array *obj, int n, ScmObj fallback);
extern float *Scm_Vec4ArrayRefv(ScmVec4Array *obj, int n);
extern void   Scm_Vec4ArraySet(ScmVec4Array *obj, int n, ScmVec4 *v);
extern void   Scm_Vec4ArraySetv(ScmVec4Array *obj, int n, float d[]);

/*=============================================================
 * Point is really a vector, with w = 1.0 by default
 */

typedef ScmVec4 ScmPoint4;

SCM_CLASS_DECL(Scm_Point4Class);
#define SCM_CLASS_POINT4       (&Scm_Point4Class)
#define SCM_POINT4P(obj)       SCM_XTYPEP(obj, SCM_CLASS_POINT4)
#define SCM_POINT4(obj)        ((ScmPoint4*)(obj))
#define SCM_POINT4_D(obj)      (SCM_POINT4(obj)->v)
#define SCM_POINT4_REF(obj, i) (SCM_POINT4_D(obj)[i])

extern ScmObj Scm_MakePoint4(float x, float y, float z, float w);
extern ScmObj Scm_MakePoint4v(const float d[]);
extern ScmObj Scm_MakePoint4V(ScmF32Vector *v);
extern ScmObj Scm_ListToPoint4(ScmObj l);
extern ScmObj Scm_Point4ToList(const ScmPoint4 *p);
extern ScmObj Scm_Point4Add(const ScmPoint4 *p, const ScmVec4 *q);
extern ScmObj Scm_Point4Sub(const ScmPoint4 *p, const ScmObj q);

/*=============================================================
 * PointArray
 */

typedef ScmVec4Array ScmPoint4Array;

SCM_CLASS_DECL(Scm_Point4ArrayClass);
#define SCM_CLASS_POINT4_ARRAY     (&Scm_Point4ArrayClass)
#define SCM_POINT4_ARRAY_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_POINT4_ARRAY)
#define SCM_POINT4_ARRAY(obj)      ((ScmPoint4Array*)(obj))
#define SCM_POINT4_ARRAY_SIZE(obj) (SCM_POINT4_ARRAY(obj)->size)
#define SCM_POINT4_ARRAY_D(obj)    (SCM_POINT4_ARRAY(obj)->v)

extern ScmObj Scm_MakePoint4Arrayv(int nvecs, const float *init);
extern ScmObj Scm_MakePoint4ArrayV(ScmF32Vector *src);

#define SCM_POINT4_ARRAY_REFV(obj, n)  (&(SCM_POINT4_ARRAY_D(obj)[(n)*4]))
#define SCM_POINT4_ARRAY_SET(obj, n, x, y, z, w)        \
   (SCM_POINT4_ARRAY_D(obj)[(n)*4] = (x),               \
    SCM_POINT4_ARRAY_D(obj)[(n)*4+1] = (y),             \
    SCM_POINT4_ARRAY_D(obj)[(n)*4+1] = (z),             \
    SCM_POINT4_ARRAY_D(obj)[(n)*4+1] = (w))

extern ScmObj Scm_Point4ArrayRef(const ScmPoint4Array *obj, int n, ScmObj fallback);
extern float *Scm_Point4ArrayRefv(ScmPoint4Array *obj, int n);
extern void   Scm_Point4ArraySet(ScmPoint4Array *obj, int n, ScmPoint4 *v);
extern void   Scm_Point4ArraySetv(ScmPoint4Array *obj, int n, float d[]);

/*=============================================================
 * Quaternions
 */

typedef struct ScmQuatRec {
    SCM_HEADER;
    float *v;
} ScmQuat;

SCM_CLASS_DECL(Scm_QuatClass);
#define SCM_CLASS_QUAT        (&Scm_QuatClass)
#define SCM_QUATP(obj)        SCM_XTYPEP(obj, SCM_CLASS_QUAT)
#define SCM_QUAT(obj)         ((ScmQuat*)(obj))
#define SCM_QUAT_D(obj)       (SCM_QUAT(obj)->v)

#define SCM_QUAT_NORMV(p)     SCM_VEC4_NORMV(p)

/* SCM_QUAT_NORMALIZE(float p[4]) */
#define SCM_QUAT_NORMALIZEV(p)                  \
    do {                                        \
        float siz__ = SCM_QUAT_NORMV(p);        \
        if (siz__ != 0.0) {                     \
            SCM_VEC4_OP(i__, p[i__] /= siz__);  \
        } else {                                \
            p[0]=p[1]=p[2]=0.0;                 \
            p[3]=1.0;                           \
        }                                       \
    } while (0)

extern ScmObj Scm_MakeQuat(float x, float y, float z, float w);
extern ScmObj Scm_MakeQuatv(const float d[4]);
extern ScmObj Scm_MakeQuatV(ScmF32Vector *v);
extern ScmObj Scm_ListToQuat(ScmObj l);
extern ScmObj Scm_QuatToList(const ScmQuat *q);

extern ScmObj Scm_QuatAdd(const ScmQuat *p, const ScmQuat *q);
extern void   Scm_QuatAddv(float *r, const float *p, const float *q);
extern ScmObj Scm_QuatSub(const ScmQuat *p, const ScmQuat *q);
extern void   Scm_QuatSubv(float *r, const float *p, const float *q);
extern ScmObj Scm_QuatMul(const ScmQuat *p, const ScmQuat *q);
extern void   Scm_QuatMulv(float *r, const float *p, const float *q);
extern ScmObj Scm_QuatNormalize(const ScmQuat *q);
extern ScmObj Scm_QuatNormalizev(float *q);
extern ScmObj Scm_QuatNormalizeX(ScmQuat *q);

extern void   Scm_QuatToMatrixv(float *r, const float *q);
extern ScmObj Scm_QuatToMatrix(const ScmQuat *q);

/*=============================================================
 * Matrix
 */

/* 4x4 matrix of floats.  The elements is stored in the same order
 * as OpenGL, that is,
 *
 *  M(0,0) = d[0] M(0,1) = d[4] M(0,2) = d[8]  M(0,3) = d[12]
 *  M(1,0) = d[1] M(1,1) = d[5] M(1,2) = d[9]  M(1,3) = d[13]
 *  M(2,0) = d[2] M(2,1) = d[6] M(2,2) = d[10] M(2,3) = d[14]
 *  M(3,0) = d[3] M(3,1) = d[7] M(3,2) = d[11] M(3,3) = d[15]
 */

typedef struct ScmMat4Rec {
    SCM_HEADER;
    float *v;
} ScmMat4;

SCM_CLASS_DECL(Scm_Mat4Class);
#define SCM_CLASS_MAT4     (&Scm_Mat4Class)
#define SCM_MAT4P(obj)     SCM_XTYPEP(obj, SCM_CLASS_MAT4)
#define SCM_MAT4(obj)      ((ScmMat4*)(obj))
#define SCM_MAT4_D(obj)    (SCM_MAT4(obj)->v)

#define SCM_MAT4_REF(obj, i, j)    (SCM_MAT4_D(obj)[(i)+(j)*4])
#define SCM_MAT4_SET(obj, i, j, v) (SCM_MAT4_D(obj)[(i)+(j)*4] = (v))
#define SCM_MAT4_COLVEC(obj, i)    (SCM_MAT4_D(obj) + (i)*4)

extern ScmObj Scm_MakeMat4v(const float d[]);
extern ScmObj Scm_ListToMat4(ScmObj l);

extern void   Scm_Mat4MulMat4v(float *, const float *, const float*);
extern ScmObj Scm_Mat4MulMat4(const ScmMat4 *, const ScmMat4 *);

extern void   Scm_Mat4MulVec4v(float *, const float *m, const float *v);
extern ScmObj Scm_Mat4MulVec4(const ScmMat4 *, const ScmVec4 *);
extern ScmObj Scm_Mat4MulPoint4(const ScmMat4 *, const ScmPoint4 *);

extern void   Scm_Mat4Scalev(float *, double f);
extern ScmObj Scm_Mat4Scale(const ScmMat4 *, double f);

#endif /* GAUCHE_MATH3D_H */
