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
 *  $Id: math3d.h,v 1.4 2002-09-27 21:11:30 shirok Exp $
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
#define SCM_3DVECTOR_OP(var, expr)              \
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

typedef struct Scm3DVectorRec {
    SCM_HEADER;
    float *v;
} Scm3DVector;

SCM_CLASS_DECL(Scm_3DVectorClass);
#define SCM_CLASS_3DVECTOR       (&Scm_3DVectorClass)
#define SCM_3DVECTORP(obj)       SCM_XTYPEP(obj, SCM_CLASS_3DVECTOR)
#define SCM_3DVECTOR(obj)        ((Scm3DVector*)(obj))
#define SCM_3DVECTOR_D(obj)      (SCM_3DVECTOR(obj)->v)
#define SCM_3DVECTOR_REF(obj, i) (SCM_3DVECTOR_D(obj)[i])

extern ScmObj Scm_Make3DVectorv(const float d[]);
extern ScmObj Scm_Make3DVector(float x, float y, float z, float w);
extern ScmObj Scm_Make3DVectorV(ScmF32Vector *v);
extern ScmObj Scm_ListTo3DVector(ScmObj l);
extern ScmObj Scm_3DVectorToList(const Scm3DVector *v);

/* SCM_3DVECTOR_DOT(float p[4], float q[4]) */
#define SCM_3DVECTOR_DOTV(p, q)  (p[0]*q[0]+p[1]*q[1]+p[2]*q[2]+p[3]*q[3])

/* SCM_3DVECTOR_CROSS(float r[4], float p[4], float q[4]); r <- p x q */
#define SCM_3DVECTOR_CROSSV(r, p, q)                \
    (r[0] = p[1]*q[2]-p[2]*q[1],                \
     r[1] = p[2]*q[0]-p[0]*q[2],                \
     r[2] = p[0]*q[1]-p[1]*q[0],                \
     r[3] = 0.0)

/* SCM_3DVECTOR_NORMV(float p[4]) */
#define SCM_3DVECTOR_NORMV(p)   sqrt(SCM_3DVECTOR_DOTV(p, p))

/* SCM_3DVECTOR_NORMALIZE(float p[4]) */
#define SCM_3DVECTOR_NORMALIZEV(p)                      \
    do {                                                \
        float siz__ = SCM_3DVECTOR_NORMV(p);            \
        if (siz__ != 0.0) {                             \
            SCM_3DVECTOR_OP(i__, p[i__] /= siz__);      \
        }                                               \
    } while (0)

/* SCM_3DVECTOR_ADD(float r[4], p[4], q[4]) */
#define SCM_3DVECTOR_ADDV(r, p, q)                \
    SCM_3DVECTOR_OP(i__, r[i__] = p[i__] + q[i__])

/* SCM_3DVECTOR_SUB(float r[4], p[4], q[4]) */
#define SCM_3DVECTOR_SUBV(r, p, q)                \
    SCM_3DVECTOR_OP(i__, r[i__] = p[i__] - q[i__])

extern float  Scm_3DVectorDot(const Scm3DVector *p, const Scm3DVector *q);
extern float  Scm_3DVectorDotv(const float *p, const float *q);
extern ScmObj Scm_3DVectorCross(const Scm3DVector *p, const Scm3DVector *q);
extern void   Scm_3DVectorCrossv(float *r, const float *p, const float *q);
extern ScmObj Scm_3DVectorNormalize(const Scm3DVector *p);
extern void   Scm_3DVectorNormalizev(float *p);
extern ScmObj Scm_3DVectorNormalizeX(Scm3DVector *p);
extern ScmObj Scm_3DVectorAdd(const Scm3DVector *p, const Scm3DVector *q);
extern void   Scm_3DVectorAddv(float *r, const float *p, const float *q);
extern ScmObj Scm_3DVectorSub(const Scm3DVector *p, const Scm3DVector *q);
extern void   Scm_3DVectorSubv(float *r, const float *p, const float *q);

/*=============================================================
 * VectorArray
 */

typedef struct Scm3DVectorArrayRec {
    SCM_HEADER;
    int size;                 /* # of vectors */
    float *v;
} Scm3DVectorArray;

SCM_CLASS_DECL(Scm_3DVectorArrayClass);
#define SCM_CLASS_3DVECTOR_ARRAY       (&Scm_3DVectorArrayClass)
#define SCM_3DVECTOR_ARRAY_P(obj)      SCM_XTYPEP(obj, SCM_CLASS_3DVECTOR_ARRAY)
#define SCM_3DVECTOR_ARRAY(obj)        ((Scm3DVectorArray*)(obj))
#define SCM_3DVECTOR_ARRAY_SIZE(obj)   (SCM_3DVECTOR_ARRAY(obj)->size)
#define SCM_3DVECTOR_ARRAY_D(obj)      (SCM_3DVECTOR_ARRAY(obj)->v)

extern ScmObj Scm_Make3DVectorArrayv(int nvecs, const float *init);
extern ScmObj Scm_Make3DVectorArrayV(ScmF32Vector *src);

#define SCM_3DVECTOR_ARRAY_REFV(obj, n)  (&(SCM_3DVECTOR_ARRAY_D(obj)[(n)*4]))
#define SCM_3DVECTOR_ARRAY_SET(obj, n, x, y, z, w)  \
   (SCM_3DVECTOR_ARRAY_D(obj)[(n)*4] = (x),         \
    SCM_3DVECTOR_ARRAY_D(obj)[(n)*4+1] = (y),       \
    SCM_3DVECTOR_ARRAY_D(obj)[(n)*4+1] = (z),       \
    SCM_3DVECTOR_ARRAY_D(obj)[(n)*4+1] = (w))

extern ScmObj Scm_3DVectorArrayRef(const Scm3DVectorArray *obj, int n, ScmObj fallback);
extern float *Scm_3DVectorArrayRefv(Scm3DVectorArray *obj, int n);
extern void   Scm_3DVectorArraySet(Scm3DVectorArray *obj, int n, Scm3DVector *v);
extern void   Scm_3DVectorArraySetv(Scm3DVectorArray *obj, int n, float d[]);

/*=============================================================
 * Point is really a vector, with w = 1.0 by default
 */

typedef Scm3DVector Scm3DPoint;

SCM_CLASS_DECL(Scm_3DPointClass);
#define SCM_CLASS_3DPOINT       (&Scm_3DPointClass)
#define SCM_3DPOINTP(obj)       SCM_XTYPEP(obj, SCM_CLASS_3DPOINT)
#define SCM_3DPOINT(obj)        ((Scm3DPoint*)(obj))
#define SCM_3DPOINT_D(obj)      (SCM_3DPOINT(obj)->v)
#define SCM_3DPOINT_REF(obj, i) (SCM_3DPOINT_D(obj)[i])

extern ScmObj Scm_Make3DPoint(float x, float y, float z, float w);
extern ScmObj Scm_Make3DPointv(const float d[]);
extern ScmObj Scm_Make3DPointV(ScmF32Vector *v);
extern ScmObj Scm_ListTo3DPoint(ScmObj l);
extern ScmObj Scm_3DPointToList(const Scm3DPoint *p);
extern ScmObj Scm_3DPointAdd(const Scm3DPoint *p, const Scm3DVector *q);
extern ScmObj Scm_3DPointSub(const Scm3DPoint *p, const ScmObj q);

/*=============================================================
 * PointArray
 */

typedef Scm3DVectorArray Scm3DPointArray;

SCM_CLASS_DECL(Scm_3DPointArrayClass);
#define SCM_CLASS_3DPOINT_ARRAY     (&Scm_3DPointArrayClass)
#define SCM_3DPOINT_ARRAY_P(obj)    SCM_XTYPEP(obj, SCM_CLASS_3DPOINT_ARRAY)
#define SCM_3DPOINT_ARRAY(obj)      ((Scm3DPointArray*)(obj))
#define SCM_3DPOINT_ARRAY_SIZE(obj) (SCM_3DPOINT_ARRAY(obj)->size)
#define SCM_3DPOINT_ARRAY_D(obj)    (SCM_3DPOINT_ARRAY(obj)->v)

extern ScmObj Scm_Make3DPointArrayv(int nvecs, const float *init);
extern ScmObj Scm_Make3DPointArrayV(ScmF32Vector *src);

#define SCM_3DPOINT_ARRAY_REFV(obj, n)  (&(SCM_3DPOINT_ARRAY_D(obj)[(n)*4]))
#define SCM_3DPOINT_ARRAY_SET(obj, n, x, y, z, w)        \
   (SCM_3DPOINT_ARRAY_D(obj)[(n)*4] = (x),               \
    SCM_3DPOINT_ARRAY_D(obj)[(n)*4+1] = (y),             \
    SCM_3DPOINT_ARRAY_D(obj)[(n)*4+1] = (z),             \
    SCM_3DPOINT_ARRAY_D(obj)[(n)*4+1] = (w))

extern ScmObj Scm_3DPointArrayRef(const Scm3DPointArray *obj, int n, ScmObj fallback);
extern float *Scm_3DPointArrayRefv(Scm3DPointArray *obj, int n);
extern void   Scm_3DPointArraySet(Scm3DPointArray *obj, int n, Scm3DPoint *v);
extern void   Scm_3DPointArraySetv(Scm3DPointArray *obj, int n, float d[]);

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

#define SCM_QUAT_NORMV(p)     SCM_3DVECTOR_NORMV(p)

/* SCM_QUAT_NORMALIZE(float p[4]) */
#define SCM_QUAT_NORMALIZEV(p)                  \
    do {                                        \
        float siz__ = SCM_QUAT_NORMV(p);        \
        if (siz__ != 0.0) {                     \
            SCM_3DVECTOR_OP(i__, p[i__] /= siz__);  \
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

typedef struct Scm3DMatrixRec {
    SCM_HEADER;
    float *v;
} Scm3DMatrix;

SCM_CLASS_DECL(Scm_3DMatrixClass);
#define SCM_CLASS_3DMATRIX     (&Scm_3DMatrixClass)
#define SCM_3DMATRIXP(obj)     SCM_XTYPEP(obj, SCM_CLASS_3DMATRIX)
#define SCM_3DMATRIX(obj)      ((Scm3DMatrix*)(obj))
#define SCM_3DMATRIX_D(obj)    (SCM_3DMATRIX(obj)->v)

#define SCM_3DMATRIX_REF(obj, i, j)    (SCM_3DMATRIX_D(obj)[(i)+(j)*4])
#define SCM_3DMATRIX_SET(obj, i, j, v) (SCM_3DMATRIX_D(obj)[(i)+(j)*4] = (v))
#define SCM_3DMATRIX_COLVEC(obj, i)    (SCM_3DMATRIX_D(obj) + (i)*4)

extern ScmObj Scm_Make3DMatrixv(const float d[]);
extern ScmObj Scm_Make3DMatrixV(ScmF32Vector *fv);
extern ScmObj Scm_ListTo3DMatrix(ScmObj l);

extern void   Scm_3DMatrixMul3DMatrixv(float *, const float *, const float*);
extern ScmObj Scm_3DMatrixMul3DMatrix(const Scm3DMatrix *, const Scm3DMatrix *);

extern void   Scm_3DMatrixMul3DVectorv(float *, const float *m, const float *v);
extern ScmObj Scm_3DMatrixMul3DVector(const Scm3DMatrix *, const Scm3DVector *);
extern ScmObj Scm_3DMatrixMul3DPoint(const Scm3DMatrix *, const Scm3DPoint *);

extern void   Scm_3DMatrixScalev(float *, double f);
extern ScmObj Scm_3DMatrixScale(const Scm3DMatrix *, double f);

#endif /* GAUCHE_MATH3D_H */
