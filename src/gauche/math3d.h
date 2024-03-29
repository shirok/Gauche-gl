/*
 * gauche/math3d.h - 3D vector and matrix arithmetic
 *
 *   Copyright (c) 2002-2023  Shiro Kawai  <shiro@acm.org>
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

/* Vector and matrix arithmetics, specialized for 3D graphics calculation. */

#ifndef GAUCHE_MATH3D_H
#define GAUCHE_MATH3D_H

#include <gauche.h>
#include <gauche/uvector.h>
#include <math.h>

/*=============================================================
 * Common utility macros
 */

/* vector operation */
#define SCM_VECTOR4F_OP(var, expr)              \
    do {                                        \
       int var;                                 \
       var = 0; { expr; }                       \
       var = 1; { expr; }                       \
       var = 2; { expr; }                       \
       var = 3; { expr; }                       \
    } while (0)

/* Check the given ScmObj val can be accepted as triple or quadraple
   of floats.  If so, sets ptr to the pointer to the floats.  Otherwise,
   report an error. */
#define SCM_MATH3D_X3FP(ptr, val)                                       \
    do {                                                                \
      if (SCM_VECTOR4FP(val))                                           \
        (ptr) = SCM_VECTOR4F_D(val);                                    \
      else if (SCM_POINT4FP(val))                                       \
        (ptr) = SCM_POINT4F_D(val);                                     \
      else if (SCM_F32VECTORP(val) && SCM_F32VECTOR_SIZE(val) >= 3)     \
        (ptr) = SCM_F32VECTOR_ELEMENTS(val);                            \
      else {                                                            \
        Scm_Error("vector4f, point4f or f32vector required,"            \
                  " but got %S", val);                                  \
        (ptr) = NULL;                                                   \
      }                                                                 \
    } while (0)

#define SCM_MATH3D_X4FP(ptr, val)                                       \
    do {                                                                \
      if (SCM_VECTOR4FP(val))                                           \
        (ptr) = SCM_VECTOR4F_D(val);                                    \
      else if (SCM_POINT4FP(val))                                       \
        (ptr) = SCM_POINT4F_D(val);                                     \
      else if (SCM_QUATFP(val))                                         \
        (ptr) = SCM_QUATF_D(val);                                       \
      else if (SCM_F32VECTORP(val) && SCM_F32VECTOR_SIZE(val) >= 4)     \
        (ptr) = SCM_F32VECTOR_ELEMENTS(val);                            \
      else {                                                            \
        Scm_Error("vector4f, point4f, quatf or f32vector required,"     \
                  " but got %S", val);                                  \
        (ptr) = NULL;                                                   \
      }                                                                 \
    } while (0)

#define SCM_MATH3D_X16FP(ptr, val)                                      \
    do {                                                                \
      if (SCM_MATRIX4FP(val))                                           \
        (ptr) = SCM_MATRIX4F_D(val);                                    \
      else if (SCM_F64VECTORP(val) && SCM_F64VECTOR_SIZE(val) >= 16)    \
        (ptr) = SCM_F64VECTOR_ELEMENTS(val);                            \
      else {                                                            \
        Scm_Error("matrix4f or f32vector of length 16 required,"        \
                  " but got %S", val);                                  \
        (ptr) = NULL;                                                   \
      }                                                                 \
    } while (0)


/* Rotation order, used in Euler->rotation conversion */
enum {
    SCM_MATH3D_ROTATE_XYZ,
    SCM_MATH3D_ROTATE_XZY,
    SCM_MATH3D_ROTATE_YZX,
    SCM_MATH3D_ROTATE_YXZ,
    SCM_MATH3D_ROTATE_ZXY,
    SCM_MATH3D_ROTATE_ZYX
};

/*=============================================================
 * 3D Vector (homogeneous coordinates)
 */

typedef struct ScmVector4fRec {
    SCM_HEADER;
    float *v;
} ScmVector4f;

SCM_CLASS_DECL(Scm_Vector4fClass);
#define SCM_CLASS_VECTOR4F       (&Scm_Vector4fClass)
#define SCM_VECTOR4FP(obj)       SCM_XTYPEP(obj, SCM_CLASS_VECTOR4F)
#define SCM_VECTOR4F(obj)        ((ScmVector4f*)(obj))
#define SCM_VECTOR4F_D(obj)      (SCM_VECTOR4F(obj)->v)
#define SCM_VECTOR4F_REF(obj, i) (SCM_VECTOR4F_D(obj)[i])

extern ScmObj Scm_MakeVector4fv(const float d[]);
extern ScmObj Scm_MakeVector4fvShared(float d[]);
extern ScmObj Scm_MakeVector4f(float x, float y, float z, float w);
extern ScmObj Scm_Vector4fSetv(ScmVector4f *v, float *d);
extern ScmObj Scm_ListToVector4f(ScmObj l);
extern ScmObj Scm_Vector4fToList(const ScmVector4f *v);

/* SCM_VECTOR4F_DOT(float p[4], float q[4]) */
#define SCM_VECTOR4F_DOTV(p, q)  (p[0]*q[0]+p[1]*q[1]+p[2]*q[2]+p[3]*q[3])

/* SCM_VECTOR4F_CROSS(float r[4], float p[4], float q[4]); r <- p x q */
#define SCM_VECTOR4F_CROSSV(r, p, q)                \
    (r[0] = p[1]*q[2]-p[2]*q[1],                \
     r[1] = p[2]*q[0]-p[0]*q[2],                \
     r[2] = p[0]*q[1]-p[1]*q[0],                \
     r[3] = 0.0)

/* SCM_VECTOR4F_NORMV(float p[4]) */
#define SCM_VECTOR4F_NORMV(p)   sqrtf(SCM_VECTOR4F_DOTV(p, p))

/* SCM_VECTOR4F_NORMALIZE(float p[4]) */
#define SCM_VECTOR4F_NORMALIZEV(p)                      \
    do {                                                \
        float siz__ = SCM_VECTOR4F_NORMV(p);            \
        if (siz__ != 0.0) {                             \
            SCM_VECTOR4F_OP(i__, p[i__] /= siz__);      \
        }                                               \
    } while (0)

/* SCM_VECTOR4F_ADD(float r[4], p[4], q[4]) */
#define SCM_VECTOR4F_ADDV(r, p, q)                \
    SCM_VECTOR4F_OP(i__, r[i__] = p[i__] + q[i__])

/* SCM_VECTOR4F_SUB(float r[4], p[4], q[4]) */
#define SCM_VECTOR4F_SUBV(r, p, q)                \
    SCM_VECTOR4F_OP(i__, r[i__] = p[i__] - q[i__])

extern float  Scm_Vector4fDot(const ScmVector4f *p, const ScmVector4f *q);
extern float  Scm_Vector4fDotv(const float *p, const float *q);
extern ScmObj Scm_Vector4fCross(const ScmVector4f *p, const ScmVector4f *q);
extern void   Scm_Vector4fCrossv(float *r, const float *p, const float *q);
extern ScmObj Scm_Vector4fNormalize(const ScmVector4f *p);
extern void   Scm_Vector4fNormalizev(float *p);
extern ScmObj Scm_Vector4fNormalizeX(ScmVector4f *p);
extern ScmObj Scm_Vector4fAdd(const ScmVector4f *p, const ScmVector4f *q);
extern void   Scm_Vector4fAddv(float *r, const float *p, const float *q);
extern ScmObj Scm_Vector4fSub(const ScmVector4f *p, const ScmVector4f *q);
extern void   Scm_Vector4fSubv(float *r, const float *p, const float *q);

/*=============================================================
 * VectorArray
 */

typedef struct ScmVector4fArrayRec {
    SCM_HEADER;
    int size;                 /* # of vectors */
    float *v;
} ScmVector4fArray;

SCM_CLASS_DECL(Scm_Vector4fArrayClass);
#define SCM_CLASS_VECTOR4F_ARRAY       (&Scm_Vector4fArrayClass)
#define SCM_VECTOR4F_ARRAY_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_VECTOR4F_ARRAY)
#define SCM_VECTOR4F_ARRAY(obj)        ((ScmVector4fArray*)(obj))
#define SCM_VECTOR4F_ARRAY_SIZE(obj)   (SCM_VECTOR4F_ARRAY(obj)->size)
#define SCM_VECTOR4F_ARRAY_D(obj)      (SCM_VECTOR4F_ARRAY(obj)->v)

extern ScmObj Scm_MakeVector4fArrayv(int nvecs, const float *init);
extern ScmObj Scm_MakeVector4fArrayV(ScmF32Vector *src);

#define SCM_VECTOR4F_ARRAY_REFV(obj, n)  (&(SCM_VECTOR4F_ARRAY_D(obj)[(n)*4]))
#define SCM_VECTOR4F_ARRAY_SET(obj, n, x, y, z, w)  \
   (SCM_VECTOR4F_ARRAY_D(obj)[(n)*4] = (x),         \
    SCM_VECTOR4F_ARRAY_D(obj)[(n)*4+1] = (y),       \
    SCM_VECTOR4F_ARRAY_D(obj)[(n)*4+2] = (z),       \
    SCM_VECTOR4F_ARRAY_D(obj)[(n)*4+3] = (w))

extern ScmObj Scm_Vector4fArrayRef(const ScmVector4fArray *obj, int n, ScmObj fallback);
extern ScmObj Scm_Vector4fArrayRefShared(ScmVector4fArray *obj, int n, ScmObj fallback);
extern float *Scm_Vector4fArrayRefv(ScmVector4fArray *obj, int n);
extern void   Scm_Vector4fArraySet(ScmVector4fArray *obj, int n, ScmVector4f *v);
extern void   Scm_Vector4fArraySetv(ScmVector4fArray *obj, int n, float d[]);

/*=============================================================
 * Point is really a vector, with w = 1.0 by default
 */

typedef ScmVector4f ScmPoint4f;

SCM_CLASS_DECL(Scm_Point4fClass);
#define SCM_CLASS_POINT4F       (&Scm_Point4fClass)
#define SCM_POINT4FP(obj)       SCM_XTYPEP(obj, SCM_CLASS_POINT4F)
#define SCM_POINT4F(obj)        ((ScmPoint4f*)(obj))
#define SCM_POINT4F_D(obj)      (SCM_POINT4F(obj)->v)
#define SCM_POINT4F_REF(obj, i) (SCM_POINT4F_D(obj)[i])

extern ScmObj Scm_MakePoint4f(float x, float y, float z, float w);
extern ScmObj Scm_MakePoint4fv(const float d[]);
extern ScmObj Scm_MakePoint4fvShared(float d[]);
extern ScmObj Scm_Point4fSetv(ScmPoint4f *v, float *d);
extern ScmObj Scm_ListToPoint4f(ScmObj l);
extern ScmObj Scm_Point4fToList(const ScmPoint4f *p);
extern ScmObj Scm_Point4fAdd(const ScmPoint4f *p, const ScmVector4f *q);
extern ScmObj Scm_Point4fSub(const ScmPoint4f *p, const ScmObj q);

/*=============================================================
 * PointArray
 */

typedef ScmVector4fArray ScmPoint4fArray;

SCM_CLASS_DECL(Scm_Point4fArrayClass);
#define SCM_CLASS_POINT4F_ARRAY     (&Scm_Point4fArrayClass)
#define SCM_POINT4F_ARRAY_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_POINT4F_ARRAY)
#define SCM_POINT4F_ARRAY(obj)      ((ScmPoint4fArray*)(obj))
#define SCM_POINT4F_ARRAY_SIZE(obj) (SCM_POINT4F_ARRAY(obj)->size)
#define SCM_POINT4F_ARRAY_D(obj)    (SCM_POINT4F_ARRAY(obj)->v)

extern ScmObj Scm_MakePoint4fArrayv(int nvecs, const float *init);
extern ScmObj Scm_MakePoint4fArrayV(ScmF32Vector *src);

#define SCM_POINT4F_ARRAY_REFV(obj, n)  (&(SCM_POINT4F_ARRAY_D(obj)[(n)*4]))
#define SCM_POINT4F_ARRAY_SET(obj, n, x, y, z, w)        \
   (SCM_POINT4F_ARRAY_D(obj)[(n)*4] = (x),               \
    SCM_POINT4F_ARRAY_D(obj)[(n)*4+1] = (y),             \
    SCM_POINT4F_ARRAY_D(obj)[(n)*4+2] = (z),             \
    SCM_POINT4F_ARRAY_D(obj)[(n)*4+3] = (w))

extern ScmObj Scm_Point4fArrayRef(const ScmPoint4fArray *obj, int n, ScmObj fallback);
extern ScmObj Scm_Point4fArrayRefShared(ScmPoint4fArray *obj, int n, ScmObj fallback);
extern float *Scm_Point4fArrayRefv(ScmPoint4fArray *obj, int n);
extern void   Scm_Point4fArraySet(ScmPoint4fArray *obj, int n, ScmPoint4f *v);
extern void   Scm_Point4fArraySetv(ScmPoint4fArray *obj, int n, float d[]);

/*=============================================================
 * Quaternions
 */

typedef struct ScmQuatfRec {
    SCM_HEADER;
    float *v;
} ScmQuatf;

SCM_CLASS_DECL(Scm_QuatfClass);
#define SCM_CLASS_QUATF        (&Scm_QuatfClass)
#define SCM_QUATFP(obj)        SCM_XTYPEP(obj, SCM_CLASS_QUATF)
#define SCM_QUATF(obj)         ((ScmQuatf*)(obj))
#define SCM_QUATF_D(obj)       (SCM_QUATF(obj)->v)

#define SCM_QUATF_NORMV(p)     SCM_VECTOR4F_NORMV(p)

/* SCM_QUATF_NORMALIZE(float p[4]) */
#define SCM_QUATF_NORMALIZEV(p)                         \
    do {                                                \
        float siz__ = SCM_QUATF_NORMV(p);               \
        if (siz__ != 0.0) {                             \
            SCM_VECTOR4F_OP(i__, p[i__] /= siz__);      \
        } else {                                        \
            p[0]=p[1]=p[2]=0.0;                         \
            p[3]=1.0;                                   \
        }                                               \
    } while (0)

#define SCM_QUATF_CONJUGATEV(q, p)              \
    do {                                        \
        q[0] = -p[0];                           \
        q[1] = -p[1];                           \
        q[2] = -p[2];                           \
        q[3] = p[3];                            \
    } while (0)

extern ScmObj Scm_MakeQuatf(float x, float y, float z, float w);
extern ScmObj Scm_MakeQuatfv(const float d[4]);
extern ScmObj Scm_MakeQuatfvShared(float d[4]);
extern ScmObj Scm_ListToQuatf(ScmObj l);
extern ScmObj Scm_QuatfToList(const ScmQuatf *q);
extern ScmObj Scm_QuatfSetv(ScmQuatf *q, const float d[4]);

extern ScmObj Scm_QuatfAdd(const ScmQuatf *p, const ScmQuatf *q);
extern void   Scm_QuatfAddv(float *r, const float *p, const float *q);
extern ScmObj Scm_QuatfSub(const ScmQuatf *p, const ScmQuatf *q);
extern void   Scm_QuatfSubv(float *r, const float *p, const float *q);
extern ScmObj Scm_QuatfMul(const ScmQuatf *p, const ScmQuatf *q);
extern void   Scm_QuatfMulv(float *r, const float *p, const float *q);
extern ScmObj Scm_QuatfNormalize(const ScmQuatf *q);
extern ScmObj Scm_QuatfNormalizeX(ScmQuatf *q);

/* q[] must be a unit quaternion */
extern void   Scm_QuatfToMatrix4fv(float *m, const float *q);

/* m[] must be an orthogonal matrix */
extern void   Scm_Matrix4fToQuatfv(float *q, const float *m);

/* q[] must be a unit quaternion */
extern void Scm_QuatfTransformv(float r[], const float q[], const float v[]);

/* p[] and q[] must be unit quaternions */
extern void   Scm_QuatfSlerp(float *r, const float *p, const float *q, float t);

/* v[], v1[], v2[], w[], w1[] and w2[] must be unit vectors */
extern void   Scm_VectorsToQuatfv(float *r, const float *v, const float *w);
extern void   Scm_AxesToQuatfv(float r[],
                               const float v1[],
                               const float v2[],
                               const float w1[],
                               const float w2[]);

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

typedef struct ScmMatrix4fRec {
    SCM_HEADER;
    float *v;
} ScmMatrix4f;

SCM_CLASS_DECL(Scm_Matrix4fClass);
#define SCM_CLASS_MATRIX4F     (&Scm_Matrix4fClass)
#define SCM_MATRIX4FP(obj)     SCM_XTYPEP(obj, SCM_CLASS_MATRIX4F)
#define SCM_MATRIX4F(obj)      ((ScmMatrix4f*)(obj))
#define SCM_MATRIX4F_D(obj)    (SCM_MATRIX4F(obj)->v)

#define SCM_MATRIX4F_REF(obj, i, j)    (SCM_MATRIX4F_D(obj)[(i)+(j)*4])
#define SCM_MATRIX4F_SET(obj, i, j, v) (SCM_MATRIX4F_D(obj)[(i)+(j)*4] = (v))
#define SCM_MATRIX4F_COLVEC(obj, i)    (SCM_MATRIX4F_D(obj) + (i)*4)

extern ScmObj Scm_MakeMatrix4fv(const float *d);
extern ScmObj Scm_MakeMatrix4fvShared(float *d);
extern ScmObj Scm_Matrix4fSetv(ScmMatrix4f *m, float *d);
extern void   Scm_Matrix4fSetIdentityv(float *p);

extern ScmObj Scm_ListToMatrix4f(ScmObj l);
extern ScmObj Scm_Matrix4fToList(const ScmMatrix4f *m);

extern void   Scm_Matrix4fMulMatrix4fv(float *, const float *, const float*);
extern ScmObj Scm_Matrix4fMulMatrix4f(const ScmMatrix4f *, const ScmMatrix4f *);

extern void   Scm_Matrix4fMulVector4fv(float *, const float *m, const float *v);
extern ScmObj Scm_Matrix4fMulVector4f(const ScmMatrix4f *, const ScmVector4f *);
extern ScmObj Scm_Matrix4fMulPoint4f(const ScmMatrix4f *, const ScmPoint4f *);

extern void   Scm_Matrix4fScalev(float *, double f);
extern ScmObj Scm_Matrix4fScale(const ScmMatrix4f *, double f);
extern void   Scm_Matrix4fTransposev(float *r, const float *m);

extern float  Scm_Matrix4fDeterminantv(const float *m);
extern int    Scm_Matrix4fInversev(float *r, const float *m);

extern void   Scm_TranslationToMatrix4fv(float *m, const float *t);
extern void   Scm_RotationToMatrix4fv(float *m, const float *v, float phi);
extern void   Scm_ScaleToMatrix4fv(float *m, const float *s);

extern void   Scm_TRSToMatrix4fv(float *m, const float *t,
                                 const float *v, float phi,
                                 const float *s);
extern void   Scm_TQSToMatrix4fv(float *m, const float *t,
                                 const float *q,
                                 const float *s);

extern void   Scm_EulerToMatrix4fv(float m[], float x, float y, float z,
                                   int order);

/* Decompose matrix m to translation vector T, rotation matrix R,
   shear vector H, and scale vector S. */
extern int    Scm_Matrix4fDecomposev(const float m[], float T[], float R[],
                                     float H[], float S[]);

/* Recover rotation from orthogonal matrix */
extern float  Scm_Matrix4fToRotationv(const float m[], float v[]);

/* Factorize Euler angles from orthogonal matrix */
extern void   Scm_Matrix4fToEuler(const float m[], float xyz[], int order);

#endif /* GAUCHE_MATH3D_H */
