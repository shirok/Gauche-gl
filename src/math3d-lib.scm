;;;
;;; math3d-lib.scm - 3d vector arithmetics
;;;
;;;  Copyright (c) 2002-2015  Shiro Kawai  <shiro@acm.org>
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(select-module gl.math3d)

(inline-stub
 (declcode (.include <math.h>
                     <gauche/uvector.h>
                     "gauche/math3d.h"))

 (include "glcase.scm")

 (declare-stub-type <vector4f> "ScmVector4f*" #f "SCM_VECTOR4FP" "SCM_VECTOR4F")
 (declare-stub-type <vector4f-array> "ScmVector4fArray*" #f
   "SCM_VECTOR4F_ARRAY_P" "SCM_VECTOR4F_ARRAY")
 (declare-stub-type <point4f> "ScmPoint4f*" #f "SCM_POINT4FP" "SCM_POINT4F")
 (declare-stub-type <point4f-array> "ScmPoint4fArray*" #f
   "SCM_POINT4F_ARRAY_P" "SCM_POINT4F_ARRAY")

 (declare-stub-type <matrix4f> "ScmMatrix4f*" #f "SCM_MATRIX4FP" "SCM_MATRIX4F")
 (declare-stub-type <quatf> "ScmQuatf*" #f "SCM_QUATFP" "SCM_QUATF")
 (declare-stub-type <f32vector> "ScmF32Vector*")

 ;; Use it as (when (index-ok? i 0 3) ...do-something...),
 ;; although if it's not ok the macro throws an error instead of returning
 ;; false.
 (define-cise-expr index-ok?
   [(_ var min max)
    `(or (and (<= ,min ,var) (<= ,var ,max))
         (begin (Scm_Error ,#`"index ,var out of range: %d" ,var) FALSE))])

 (define-cise-expr index-ok/fallback?
   [(_ var min max fallback-var)
    `(or (and (<= ,min ,var) (<= ,var ,max))
         (and (SCM_UNBOUNDP ,fallback-var)
              (begin (Scm_Error ,#`"index ,var out of range: %d" ,var) FALSE))
         (begin (result ,fallback-var) FALSE))])

 ;; Check the 'start' offset against the uniform vector vec.
 ;; Again, you can use it as (when (start-ok? ...) ...) but the failure
 ;; case throws an error instead of returning false.
 (define-cise-expr start-ok?
   [(_ start vec count)
    `(or (and (<= 0 ,start) (<= (+ ,start ,count) (SCM_UVECTOR_SIZE ,vec)))
         (begin (Scm_Error "uvector too small: %S (start=%d)" ,vec ,start)
                FALSE))])
 )

;;================================================================
;; Vector4f
;;

(define-cproc vector4f (x::<float> y::<float> z::<float> &optional (w::<float> 0.0))
  Scm_MakeVector4f)
(define-cproc vector4f? (obj) ::<boolean> SCM_VECTOR4FP)
(define-cproc make-vector4f () (result (Scm_MakeVector4fv NULL)))
(define-cproc list->vector4f (l::<list>) Scm_ListToVector4f)
(define-cproc vector4f->list (v::<vector4f>) Scm_Vector4fToList)

(define-cproc f32vector->vector4f (v::<f32vector>
                                   &optional (start::<fixnum> 0))
  (when (start-ok? start v 4)
    (result (Scm_MakeVector4fv (+ (SCM_F32VECTOR_ELEMENTS v) start)))))

(define-cproc vector4f->f32vector (v::<vector4f>)
  (result (Scm_MakeF32VectorFromArray 4 (SCM_VECTOR4F_D v))))

(define-cproc vector4f-copy (v::<vector4f>)
  (result (Scm_MakeVector4fv (SCM_VECTOR4F_D v))))

(define-cproc vector4f-copy! (dst::<vector4f> src::<vector4f>)
  (result (Scm_Vector4fSetv dst (SCM_VECTOR4F_D src))))

(define-cproc vector4f-set! (x::<vector4f> i::<fixnum> v::<float>) ::<void>
  (when (index-ok? i 0 3) (set! (aref (SCM_VECTOR4F_D x) i) v)))

(define-cproc vector4f-ref (x::<vector4f> i::<fixnum> &optional fallback)
  (setter vector4f-set!)
  (when (index-ok/fallback? i 0 3 fallback)
    (result (Scm_MakeFlonum (SCM_VECTOR4F_REF x i)))))

(define-cproc vector4f-dot (x::<vector4f> y::<vector4f>) ::<float>
  Scm_Vector4fDot)

(define-cproc vector4f-cross (x::<vector4f> y::<vector4f>) Scm_Vector4fCross)

(define-cproc vector4f-norm (v::<vector4f>) ::<double>
  (result (SCM_VECTOR4F_NORMV (SCM_VECTOR4F_D v))))

(define-cproc vector4f-normalize (x::<vector4f>) Scm_Vector4fNormalize)
(define-cproc vector4f-normalize! (x::<vector4f>) Scm_Vector4fNormalizeX)

(define-cproc vector4f-add (x::<vector4f> y::<vector4f>) Scm_Vector4fAdd)
(define-cproc vector4f-add! (x::<vector4f> y::<vector4f>)
  (Scm_Vector4fAddv (SCM_VECTOR4F_D x) (SCM_VECTOR4F_D x) (SCM_VECTOR4F_D y))
  (result (SCM_OBJ x)))

(define-cproc vector4f-sub (x::<vector4f> y::<vector4f>) Scm_Vector4fSub)
(define-cproc vector4f-sub! (x::<vector4f> y::<vector4f>)
  (Scm_Vector4fSubv (SCM_VECTOR4F_D x) (SCM_VECTOR4F_D x) (SCM_VECTOR4F_D y))
  (result (SCM_OBJ x)))

(inline-stub
 (define-cise-stmt vec4-eltwise
   [(_ stmts ...)
    (letrec ((replace (lambda (form i)
                        (match form
                          ['_ i]
                          [(xs ...) (map (cut replace <> i) xs)]
                          [_ form]))))
      `(begin ,@(append-map (lambda (stmt)
                              (map (cute replace stmt <>) '(0 1 2 3)))
                            stmts)))])
 )

(define-cproc vector4f-mul (x::<vector4f> f::<float>)
  (let* ([r::(.array float (4))])
    (vec4-eltwise (set! (aref r _) (* (SCM_VECTOR4F_REF x _) f)))
    (result (Scm_MakeVector4fv r))))

(define-cproc vector4f-mul! (x::<vector4f> f::<float>)
  (vec4-eltwise (set! (SCM_VECTOR4F_REF x _) (* (SCM_VECTOR4F_REF x _) f)))
  (result (SCM_OBJ x)))

(define-cproc vector4f-div (x::<vector4f> f::<float>)
  (let* ([r::(.array float [4])])
    (vec4-eltwise (set! (aref r _) (/ (SCM_VECTOR4F_REF x _) f)))
    (result (Scm_MakeVector4fv r))))

(define-cproc vector4f-div! (x::<vector4f> f::<float>)
  (vec4-eltwise (set! (SCM_VECTOR4F_REF x _) (/ (SCM_VECTOR4F_REF x _) f)))
  (result (SCM_OBJ x)))

;; VectorArray --------------------------------------------------

(define-cproc make-vector4f-array (len::<fixnum> &optional init)
  (when (< len 0) (Scm_Error "vector4f-array length must be positive: %d" len))
  (cond [(SCM_VECTOR4FP init)
         (result (Scm_MakeVector4fArrayv len (SCM_VECTOR4F_D init)))]
        [(SCM_UNBOUNDP init)
         (result (Scm_MakeVector4fArrayv len NULL))]
        [else
         (Scm_Error "bad initializer for vector array: must be <vector4f>, but got %S" init)]))

(define-cproc vector4f-array? (obj) ::<boolean> SCM_VECTOR4F_ARRAY_P)

(define-cproc vector4f-array-length (v::<vector4f-array>) ::<fixnum>
  SCM_VECTOR4F_ARRAY_SIZE)

(define-cproc f32vector->vector4f-array/shared (v::<f32vector>)
  Scm_MakeVector4fArrayV)

(define-cproc vector4f-array->f32vector (a::<vector4f-array>)
  (result (Scm_MakeF32VectorFromArray (* (SCM_VECTOR4F_ARRAY_SIZE a) 4)
                                      (SCM_VECTOR4F_ARRAY_D a))))

(define-cproc vector4f-array-set! (a::<vector4f-array>
                                   i::<fixnum> x::<vector4f>)
  ::<void> Scm_Vector4fArraySet)

(define-cproc vector4f-array-ref (a::<vector4f-array>
                                  i::<fixnum> &optional fallback)
  (setter vector4f-array-set!)
  Scm_Vector4fArrayRef)

(define-cproc vector4f-array-ref/shared (a::<vector4f-array>
                                         i::<fixnum> &optional fallback)
  Scm_Vector4fArrayRefShared)

;; point4f ------------------------------------------------------

(define-cproc point4f (x::<float> y::<float> z::<float> &optional (w::<float> 1.0))
  Scm_MakePoint4f)
(define-cproc point4f? (obj) ::<boolean> SCM_POINT4FP)
(define-cproc make-point4f () (result (Scm_MakePoint4fv NULL)))
(define-cproc list->point4f (l::<list>) Scm_ListToPoint4f)
(define-cproc point4f->list (x::<point4f>) Scm_Point4fToList)

(define-cproc f32vector->point4f (v::<f32vector> &optional (start::<fixnum> 0))
  (when (start-ok? start v 4)
    (result (Scm_MakePoint4fv (+ (SCM_F32VECTOR_ELEMENTS v) start)))))

(define-cproc point4f->f32vector (v::<point4f>)
  (result (Scm_MakeF32VectorFromArray 4 (SCM_POINT4F_D v))))

(define-cproc point4f-copy (v::<point4f>)
  (result (Scm_MakePoint4fv (SCM_POINT4F_D v))))

(define-cproc point4f-copy! (dst::<point4f> src::<point4f>)
  (result (Scm_Point4fSetv dst (SCM_POINT4F_D src))))

(define-cproc point4f-set! (x::<point4f> i::<fixnum> v::<float>) ::<void>
  (when (index-ok? i 0 3) (set! (aref (SCM_POINT4F_D x) i) v)))

(define-cproc point4f-ref (x::<point4f> i::<fixnum>)
  (setter point4f-set!)
  (when (index-ok? i 0 3) (result (Scm_MakeFlonum (SCM_POINT4F_REF x i)))))

(define-cproc point4f-add (x::<point4f> y::<vector4f>) Scm_Point4fAdd)

(define-cproc point4f-add! (x::<point4f> y::<vector4f>)
  (Scm_Vector4fAddv (SCM_POINT4F_D x) (SCM_POINT4F_D x) (SCM_VECTOR4F_D y))
  (result (SCM_OBJ x)))

(define-cproc point4f-sub (x::<point4f> y) Scm_Point4fSub)

;point4f-sub!

;; Point4fArray ---------------------------------------------------

(define-cproc make-point4f-array (len::<fixnum> &optional init)
  (when (< len 0) (Scm_Error "point4f-array length must be positive: %d" len))
  (cond [(SCM_POINT4FP init)
         (result (Scm_MakePoint4fArrayv len (SCM_POINT4F_D init)))]
        [(SCM_UNBOUNDP init)
         (result (Scm_MakePoint4fArrayv len NULL))]
        [else
         (Scm_Error "bad initializer for point array: must be <point4f>, but got %S" init)]))

(define-cproc point4f-array? (obj) ::<boolean> SCM_POINT4F_ARRAY_P)

(define-cproc point4f-array-length (v::<point4f-array>)::<fixnum>
  SCM_POINT4F_ARRAY_SIZE)

(define-cproc f32vector->point4f-array/shared (v::<f32vector>)
  Scm_MakePoint4fArrayV)

(define-cproc point4f-array->f32vector (a::<point4f-array>)
  (result (Scm_MakeF32VectorFromArray (* (SCM_POINT4F_ARRAY_SIZE a) 4)
                                      (SCM_POINT4F_ARRAY_D a))))

(define-cproc point4f-array-set! (a::<point4f-array>
                                  i::<fixnum>
                                  x::<point4f>)
  ::<void> Scm_Point4fArraySet)

(define-cproc point4f-array-ref (a::<point4f-array>
                                 i::<fixnum> &optional fallback)
  (setter point4f-array-set!)
  Scm_Point4fArrayRef)

(define-cproc point4f-array-ref/shared (a::<point4f-array>
                                        i::<fixnum> &optional fallback)
  Scm_Point4fArrayRefShared)

;; Matrix4f -------------------------------------------------------

(inline-stub
 "static float matrix4f_unit[] = { 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0 };")

(define-cproc make-matrix4f (&optional init)
  (if (SCM_UNBOUNDP init)
    (result (Scm_MakeMatrix4fv matrix4f_unit))
    (begin (assert-vector-type&size f32 16 init)
           (result (Scm_MakeMatrix4fv (SCM_F32VECTOR_ELEMENTS init))))))

(define-cproc matrix4f (&rest args) Scm_ListToMatrix4f)
(define-cproc matrix4f? (obj)::<boolean> SCM_MATRIX4FP)
(define-cproc list->matrix4f (l::<list>) Scm_ListToMatrix4f)
(define-cproc matrix4f->list (m::<matrix4f>) Scm_Matrix4fToList)

(define-cproc f32vector->matrix4f (v::<f32vector>
                                   &optional (start::<fixnum> 0))
  (when (start-ok? start v 16)
    (result (Scm_MakeMatrix4fv (+ (SCM_F32VECTOR_ELEMENTS v) start)))))

(define-cproc f32vector->matrix4f! (m::<matrix4f>
                                    v::<f32vector>
                                    &optional (start::<fixnum> 0))
  (when (start-ok? start v 16)
    (Scm_Matrix4fSetv m (+ (SCM_F32VECTOR_ELEMENTS v) start))
    (result (SCM_OBJ m))))

(define-cproc matrix4f->f32vector (m::<matrix4f>)
  (result (Scm_MakeF32VectorFromArray 16 (SCM_MATRIX4F_D m))))

(define-cproc matrix4f-copy (m::<matrix4f>)
  (result (Scm_MakeMatrix4fv (SCM_MATRIX4F_D m))))
(define-cproc matrix4f-copy! (dst::<matrix4f> src::<matrix4f>)
  (result (Scm_Matrix4fSetv dst (SCM_MATRIX4F_D src))))

(define-cproc matrix4f-mul (p::<matrix4f> q)
  (cond
   [(SCM_MATRIX4FP q) (result (Scm_Matrix4fMulMatrix4f p (SCM_MATRIX4F q)))]
   [(SCM_VECTOR4FP q) (result (Scm_Matrix4fMulVector4f p (SCM_VECTOR4F q)))]
   [(SCM_POINT4FP q) (result (Scm_Matrix4fMulPoint4f p (SCM_POINT4F q)))]
   [(SCM_REALP q) (result (Scm_Matrix4fScale p (Scm_GetDouble q)))]
   [else (Scm_Error "bad object, matrix4f, vector4f, point4f or real number required, but got %S" q)]))

(define-cproc matrix4f-mul! (p::<matrix4f> q::<matrix4f>)
  (let* ([r::(.array float (16))])
    (Scm_Matrix4fMulMatrix4fv r (SCM_MATRIX4F_D p) (SCM_MATRIX4F_D q))
    (Scm_Matrix4fSetv p r)
    (result (SCM_OBJ p))))

(define-cproc matrix4f-transpose (m::<matrix4f>)
  (let* ([r::(.array float (16))])
    (Scm_Matrix4fTransposev r (SCM_MATRIX4F_D m))
    (result (Scm_MakeMatrix4fv r))))

(define-cproc matrix4f-transpose! (m::<matrix4f>)
  (let* ([r::(.array float (16))] [p::float* (SCM_MATRIX4F_D m)])
    (Scm_Matrix4fTransposev r p)
    (Scm_Matrix4fSetv m r)
    (result (SCM_OBJ m))))

(define-cproc matrix4f-set! (m::<matrix4f> i::<fixnum> v::<float>) ::<void>
  (when (index-ok? i 0 15) (set! (aref (SCM_MATRIX4F_D m) i) v)))

(define-cproc matrix4f-set-identity! (m::<matrix4f>) ::<void>
  (Scm_Matrix4fSetIdentityv (SCM_MATRIX4F_D m)))

(define-cproc matrix4f-ref (m::<matrix4f> i::<fixnum> &optional fallback)
  (setter matrix4f-set!)
  (when (index-ok/fallback? i 0 15 fallback)
    (result (Scm_MakeFlonum (aref (SCM_MATRIX4F_D m) i)))))

(define-cproc matrix4f-set2! (m::<matrix4f> i::<fixnum> j::<fixnum> v::<float>)
  ::<void>
  (when (index-ok? i 0 3)
    (when (index-ok? j 0 3)
      (SCM_MATRIX4F_SET m i j v))))

(define-cproc matrix4f-ref2 (m::<matrix4f> i::<fixnum> j::<fixnum>) ::<float>
  (setter matrix4f-set2!)
  (when (index-ok? i 0 3)
    (when (index-ok? j 0 3)
      (result (SCM_MATRIX4F_REF m i j)))))

(define-cproc matrix4f-row (m::<matrix4f> i::<fixnum>)
  (setter (m::<matrix4f> i::<fixnum> v) ::<void>
    (when (index-ok? i 0 3)
      (let* ([fv::float*])
        (SCM_MATH3D_X4FP fv v)
        (SCM_MATRIX4F_SET m i 0 (aref fv 0))
        (SCM_MATRIX4F_SET m i 1 (aref fv 1))
        (SCM_MATRIX4F_SET m i 2 (aref fv 2))
        (SCM_MATRIX4F_SET m i 3 (aref fv 3)))))
  (when (index-ok? i 0 3)
    (result (Scm_MakeVector4f (SCM_MATRIX4F_REF m i 0)
                              (SCM_MATRIX4F_REF m i 1)
                              (SCM_MATRIX4F_REF m i 2)
                              (SCM_MATRIX4F_REF m i 3)))))

(define-cproc matrix4f-column (m::<matrix4f> i::<fixnum>)
  (setter (m::<matrix4f> i::<fixnum> v) ::<void>
    (when (index-ok? i 0 3)
      (let* ([fv::float*])
        (SCM_MATH3D_X4FP fv v)
        (SCM_MATRIX4F_SET m 0 i (aref fv 0))
        (SCM_MATRIX4F_SET m 1 i (aref fv 1))
        (SCM_MATRIX4F_SET m 2 i (aref fv 2))
        (SCM_MATRIX4F_SET m 3 i (aref fv 3)))))
  (when (index-ok? i 0 3)
    (result (Scm_MakeVector4fv (SCM_MATRIX4F_COLVEC m i)))))

(define-cproc matrix4f-column/shared (m::<matrix4f> i::<fixnum>)
  (when (index-ok? i 0 3)
    (result (Scm_MakeVector4fvShared (SCM_MATRIX4F_COLVEC m i)))))

;; determinant and inverse
(define-cproc matrix4f-determinant (m::<matrix4f>) ::<double>
  (result (Scm_Matrix4fDeterminantv (SCM_MATRIX4F_D m))))

(define-cproc matrix4f-inverse (m::<matrix4f> &optional (error-on-singular #t))
  (let* ([r (Scm_MakeMatrix4fv NULL)]
         [code::int
          (Scm_Matrix4fInversev (SCM_MATRIX4F_D r) (SCM_MATRIX4F_D m))])
    (when (and (not code) (not (SCM_FALSEP error-on-singular)))
      (Scm_Error "attempt to inverse singular matrix: %S" m))
    (result (?: code r SCM_FALSE))))

(define-cproc matrix4f-inverse! (m::<matrix4f> &optional (error-on-singular #t))
  (let* ([r::(.array float (16))]
         [code::int (Scm_Matrix4fInversev r (SCM_MATRIX4F_D m))])
    (when (and (not code) (not (SCM_FALSEP error-on-singular)))
      (Scm_Error "attempt to inverse singular matrix: %S" m))
    (cond [code
           (dotimes [i 16] (set! (aref (SCM_MATRIX4F_D m) i) (aref r i)))
           (result (SCM_OBJ m))]
          [else (result '#f)])))

;; basic transformation

(define-cproc translation->matrix4f (t)
  (let* ([r::(.array float [16])] [p::float*])
    (SCM_MATH3D_X3FP p t)
    (Scm_TranslationToMatrix4fv r p)
    (result (Scm_MakeMatrix4fv r))))
(define-cproc translation->matrix4f! (m::<matrix4f> t)
  (let* ([p::float*])
    (SCM_MATH3D_X3FP p t)
    (Scm_TranslationToMatrix4fv (SCM_MATRIX4F_D m) p)
    (result (SCM_OBJ m))))

(define-cproc rotation->matrix4f (v angle::<float>)
  (let* ([r::(.array float [16])] [p::float*])
    (SCM_MATH3D_X3FP p v)
    (Scm_RotationToMatrix4fv r p angle)
    (result (Scm_MakeMatrix4fv r))))
(define-cproc rotation->matrix4f! (m::<matrix4f> v angle::<float>)
  (let* ([p::float*])
    (SCM_MATH3D_X3FP p v)
    (Scm_RotationToMatrix4fv (SCM_MATRIX4F_D m) p angle)
    (result (SCM_OBJ m))))

(define-cproc scale->matrix4f (s)
  (let* ([r::(.array float[16])] [p::float* NULL])
    (SCM_MATH3D_X3FP p s)
    (Scm_ScaleToMatrix4fv r p)
    (result (Scm_MakeMatrix4fv r))))
(define-cproc scale->matrix4f! (m::<matrix4f> s)
  (let* ([p::float*])
    (SCM_MATH3D_X3FP p s)
    (Scm_ScaleToMatrix4fv (SCM_MATRIX4F_D m) p)
    (result (SCM_OBJ m))))

(define-cproc trs->matrix4f (t v angle::<float> s)
  (let* ([r::(.array float [16])] [pt::float*] [pv::float*] [ps::float*])
    (SCM_MATH3D_X3FP pt t)
    (SCM_MATH3D_X3FP pv v)
    (SCM_MATH3D_X3FP ps s)
    (Scm_TRSToMatrix4fv r pt pv angle ps)
    (result (Scm_MakeMatrix4fv r))))
(define-cproc trs->matrix4f! (m::<matrix4f> t v angle::<float> s)
  (let* ([pt::float*] [pv::float*] [ps::float*])
    (SCM_MATH3D_X3FP pt t)
    (SCM_MATH3D_X3FP pv v)
    (SCM_MATH3D_X3FP ps s)
    (Scm_TRSToMatrix4fv (SCM_MATRIX4F_D m) pt pv angle ps)
    (result (SCM_OBJ m))))

(define-cproc tqs->matrix4f (t q s)
  (let* ([r::(.array float [16])] [pt::float*] [pq::float*] [ps::float*])
    (SCM_MATH3D_X3FP pt t)
    (SCM_MATH3D_X4FP pq q)
    (SCM_MATH3D_X3FP ps s)
    (Scm_TQSToMatrix4fv r pt pq ps)
    (result (Scm_MakeMatrix4fv r))))
(define-cproc tqs->matrix4f! (m::<matrix4f> t q s)
  (let* ([pt::float*] [pq::float*] [ps::float*])
    (SCM_MATH3D_X3FP pt t)
    (SCM_MATH3D_X4FP pq q)
    (SCM_MATH3D_X3FP ps s)
    (Scm_TQSToMatrix4fv (SCM_MATRIX4F_D m) pt pq ps)
    (result (SCM_OBJ m))))

(define-cfn rotation-order (sym) ::int
  (cond [(SCM_UNBOUNDP sym) (return SCM_MATH3D_ROTATE_XYZ)] ;; default
        [(SCM_EQ sym 'xyz)  (return SCM_MATH3D_ROTATE_XYZ)]
        [(SCM_EQ sym 'xzy)  (return SCM_MATH3D_ROTATE_XZY)]
        [(SCM_EQ sym 'yzx)  (return SCM_MATH3D_ROTATE_YZX)]
        [(SCM_EQ sym 'yxz)  (return SCM_MATH3D_ROTATE_YXZ)]
        [(SCM_EQ sym 'zxy)  (return SCM_MATH3D_ROTATE_ZXY)]
        [(SCM_EQ sym 'zyx)  (return SCM_MATH3D_ROTATE_ZYX)]
        [else (Scm_Error "bad rotation order: must be either one of xyz, xzy, yzx, yxz, zxy, or zyx, but got %S" sym)
              (return 0)]))             ;dummy

(define-cproc euler-angle->matrix4f (x::<float> y::<float> z::<float>
                                     &optional order)
  (let* ([m::(.array float[16])])
    (Scm_EulerToMatrix4fv m x y z (rotation_order order))
    (result (Scm_MakeMatrix4fv m))))

(define-cproc euler-angle->matrix4f! (m::<matrix4f>
                                      x::<float> y::<float> z::<float>
                                     &optional order)
  ::<void>
  (Scm_EulerToMatrix4fv (SCM_MATRIX4F_D m) x y z (rotation_order order)))

(define-cproc matrix4f-decompose (m::<matrix4f>)
  ::(<boolean> <top> <top> <top> <top>)
  (let* ([T::(.array float[4])] [S::(.array float[4])]
         [H::(.array float[4])] [R::(.array float[16])]
         [r::int (Scm_Matrix4fDecomposev (SCM_MATRIX4F_D m) T R H S)])
    ;; NB: This should be
    ;; (result r (Scm_MakeVector4fv T) (Scm_MakeMatrix4fv R)
    ;;           (Scm_MakeVector4fv H) (Scm_MakeVector4fv S))))
    ;; but 0.9.3.3's cise doesn't support this many return values.
    (set! SCM_RESULT0 r
          SCM_RESULT1 (Scm_MakeVector4fv T)
          SCM_RESULT2 (Scm_MakeMatrix4fv R)
          SCM_RESULT3 (Scm_MakeVector4fv H)
          SCM_RESULT4 (Scm_MakeVector4fv S))))

(define-cproc matrix4f-decompose! (m::<matrix4f> T::<vector4f> R::<matrix4f>
                                   H::<vector4f> S::<vector4f>)
  ::<boolean>
  (result (Scm_Matrix4fDecomposev (SCM_MATRIX4F_D m)
                                  (SCM_VECTOR4F_D T)
                                  (SCM_MATRIX4F_D R)
                                  (SCM_VECTOR4F_D H)
                                  (SCM_VECTOR4F_D S))))

(define-cproc matrix4f->translation (m::<matrix4f>)
  (result (Scm_MakeVector4f (aref (SCM_MATRIX4F_D m) 12)
                            (aref (SCM_MATRIX4F_D m) 13)
                            (aref (SCM_MATRIX4F_D m) 14)
                            0.0)))

(define-cproc matrix4f->translation! (v::<vector4f> m::<matrix4f>)
  (set! (SCM_VECTOR4F_REF v 0) (aref (SCM_MATRIX4F_D m) 12))
  (set! (SCM_VECTOR4F_REF v 1) (aref (SCM_MATRIX4F_D m) 13))
  (set! (SCM_VECTOR4F_REF v 2) (aref (SCM_MATRIX4F_D m) 14))
  (set! (SCM_VECTOR4F_REF v 3) 0.0)
  (result (SCM_OBJ v)))

(define-cproc matrix4f->rotation (m::<matrix4f>) ::(<top> <float>)
  (let* ([v::(.array float[4])]
         [angle::float (Scm_Matrix4fToRotationv (SCM_MATRIX4F_D m) v)])
    (result (Scm_MakeVector4fv v) angle)))

(define-cproc matrix4f->rotation! (v::<vector4f> m::<matrix4f>)
  ::(<top> <float>)
  (let* ([angle::float (Scm_Matrix4fToRotationv (SCM_MATRIX4F_D m)
                                                (SCM_VECTOR4F_D v))])
    (result (SCM_OBJ v) angle)))

;; projection matrix
(define (ortho->matrix4f left right bottom top near far)
  (rlet1 m (make-matrix4f)
    (ortho->matrix4f! m left right bottom top near far)))

(define-cproc ortho->matrix4f! (m::<matrix4f>
                                left::<float> right::<float>
                                bottom::<float> top::<float>
                                nearVal::<float> farVal::<float>)
  ::<matrix4f>
  (let* ([r_l::float (- right left)]
         [t_b::float (- top bottom)]
         [f_n::float (- farVal nearVal)])
    (set! (aref (SCM_MATRIX4F_D m) 0) (/ 2.0 r_l))
    (set! (aref (SCM_MATRIX4F_D m) 1) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 2) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 3) 0.0)

    (set! (aref (SCM_MATRIX4F_D m) 4) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 5) (/ 2.0 t_b))
    (set! (aref (SCM_MATRIX4F_D m) 6) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 7) 0.0)

    (set! (aref (SCM_MATRIX4F_D m) 8) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 9) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 10) (/ 2.0 f_n))
    (set! (aref (SCM_MATRIX4F_D m) 11) 0.0)

    (set! (aref (SCM_MATRIX4F_D m) 12) (- (/ (+ right left) r_l)))
    (set! (aref (SCM_MATRIX4F_D m) 13) (- (/ (+ top bottom) t_b)))
    (set! (aref (SCM_MATRIX4F_D m) 14) (- (/ (+ farVal nearVal) f_n)))
    (set! (aref (SCM_MATRIX4F_D m) 15) 1.0)
    (return m)))

(define (frustum->matrix4f left right bottom top near far)
  (rlet1 m (make-matrix4f)
    (frustum->matrix4f! m left right bottom top near far)))

(define-cproc frustum->matrix4f! (m::<matrix4f>
                                left::<float> right::<float>
                                bottom::<float> top::<float>
                                nearVal::<float> farVal::<float>)
  ::<matrix4f>
  (let* ([r_l::float (- right left)]
         [t_b::float (- top bottom)]
         [f_n::float (- farVal nearVal)])
    (set! (aref (SCM_MATRIX4F_D m) 0) (/ (* 2.0 nearVal) r_l))
    (set! (aref (SCM_MATRIX4F_D m) 1) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 2) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 3) 0.0)

    (set! (aref (SCM_MATRIX4F_D m) 4) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 5) (/ (* 2.0 nearVal) t_b))
    (set! (aref (SCM_MATRIX4F_D m) 6) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 7) 0.0)

    (set! (aref (SCM_MATRIX4F_D m) 8) (/ (+ right left) r_l))
    (set! (aref (SCM_MATRIX4F_D m) 9) (/ (+ top bottom) t_b))
    (set! (aref (SCM_MATRIX4F_D m) 10) (- (/ (+ farVal nearVal) f_n)))
    (set! (aref (SCM_MATRIX4F_D m) 11) -1.0)

    (set! (aref (SCM_MATRIX4F_D m) 12) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 13) 0.0)
    (set! (aref (SCM_MATRIX4F_D m) 14) (- (/ (* 2 farVal nearVal) f_n)))
    (set! (aref (SCM_MATRIX4F_D m) 15) 1.0)
    (return m)))

;; Quatf ----------------------------------------------------

(define-cproc quatf (x::<float> y::<float> z::<float> w::<float>) Scm_MakeQuatf)

(define-cproc quatf? (obj) ::<boolean> SCM_QUATFP)

(define-cproc make-quatf (&optional vec (angle::<float> 0))
  (if (SCM_UNBOUNDP vec)
    (result (Scm_MakeQuatf 0.0 0.0 0.0 1.0))
    (let* ([q::float*])
      (SCM_MATH3D_X3FP q vec)
      (let* ([sint::double (sin (/ angle 2.0))]
             [cost::double (cos (/ angle 2.0))])
        (result (Scm_MakeQuatf (* sint (aref q 0))
                               (* sint (aref q 1))
                               (* sint (aref q 2))
                               cost))))))

(define-cproc list->quatf (x) Scm_ListToQuatf)
(define-cproc quatf->list (q::<quatf>) Scm_QuatfToList)
(define-cproc f32vector->quatf (x::<f32vector>
                                &optional (start::<fixnum> 0))
  (when (start-ok? start x 4)
    (result (Scm_MakeQuatfv (+ (SCM_F32VECTOR_ELEMENTS x) start)))))
(define-cproc quatf->f32vector (q::<quatf>)
  (result (Scm_MakeF32VectorFromArray 4 (SCM_QUATF_D q))))

(define-cproc quatf-copy (q::<quatf>)
  (result (Scm_MakeQuatfv (SCM_QUATF_D q))))
(define-cproc quatf-copy! (dst::<quatf> src::<quatf>)
  (result (Scm_QuatfSetv dst (SCM_QUATF_D src))))

(define-cproc quatf-ref (q::<quatf> i::<fixnum> &optional fallback)
  (when (index-ok/fallback? i 0 3 fallback)
    (result (Scm_MakeFlonum (aref (SCM_QUATF_D q) i)))))

(define-cproc quatf-set! (q::<quatf> i::<fixnum> val::<float>)
  (when (index-ok? i 0 3)
    (set! (aref (SCM_QUATF_D q) i) (cast float val))
    (result (SCM_OBJ q))))

(define-cproc quatf-set4! (q::<quatf> x::<float> y::<float> z::<float> w::<float>)
  (set! (aref (SCM_QUATF_D q) 0) x
        (aref (SCM_QUATF_D q) 1) y
        (aref (SCM_QUATF_D q) 2) z
        (aref (SCM_QUATF_D q) 3) w)
  (result (SCM_OBJ q)))

(define-cproc rotation->quatf! (q::<quatf> v angle::<float>)
  (let* ([qv::float* (SCM_QUATF_D q)] [vv::float*])
    (SCM_MATH3D_X3FP vv v)
    (let* ([sint::double (sin (/ angle 2.0))]
           [cost::double (cos (/ angle 2.0))])
      (set! (aref qv 0) (* sint (aref vv 0)))
      (set! (aref qv 1) (* sint (aref vv 1)))
      (set! (aref qv 2) (* sint (aref vv 2)))
      (set! (aref qv 3) cost)))
  (result (SCM_OBJ q)))

(define-cproc quatf-add (p::<quatf> q::<quatf>) Scm_QuatfAdd)
(define-cproc quatf-add! (p::<quatf> q::<quatf>)
  (let* ([r::(.array float [4])])
    (Scm_QuatfAddv r (SCM_QUATF_D p) (SCM_QUATF_D q))
    (result (Scm_QuatfSetv p r))))

(define-cproc quatf-sub (p::<quatf> q::<quatf>) Scm_QuatfSub)
(define-cproc quatf-sub! (p::<quatf> q::<quatf>)
  (let* ([r::(.array float [4])])
    (Scm_QuatfSubv r (SCM_QUATF_D p) (SCM_QUATF_D q))
    (result (Scm_QuatfSetv p r))))

(define-cproc quatf-scale (q::<quatf> s::<float>)
  (let* ([d::float* (SCM_QUATF_D q)])
    (when (== s 0.0) (Scm_Error "divide by zero"))
    (result (Scm_MakeQuatf (/ (aref d 0) s) (/ (aref d 1) s)
                           (/ (aref d 2) s) (/ (aref d 3) s)))))
(define-cproc quatf-scale! (q::<quatf> s::<float>)
  (let* ([d::float* (SCM_QUATF_D q)])
    (when (== s 0.0) (Scm_Error "divide by zero"))
    (dotimes [i 4] (/= (aref d i) s))
    (result (SCM_OBJ q))))

(define-cproc quatf-mul (p::<quatf> q::<quatf>) Scm_QuatfMul)
(define-cproc quatf-mul! (p::<quatf> q::<quatf>)
  (let* ([r::(.array float [4])])
    (Scm_QuatfMulv r (SCM_QUATF_D p) (SCM_QUATF_D q))
    (result (Scm_QuatfSetv p r))))

;; calculate qpq*
(define-cproc quatf-transform (quat::<quatf> v)
  (let* ([d::float*] [r::(.array float [4])])
    (SCM_MATH3D_X4FP d v)
    (Scm_QuatfTransformv r (SCM_QUATF_D quat) d)
    (cond [(SCM_VECTOR4FP v) (result (Scm_MakeVector4fv r))]
          [(SCM_POINT4FP v)  (result (Scm_MakePoint4fv r))]
          [else (result (Scm_MakeF32VectorFromArray 4 r))])))

(define-cproc quatf-conjugate (q::<quatf>)
  (let* ([d::float* (SCM_QUATF_D q)])
    (result (Scm_MakeQuatf (- (aref d 0)) (- (aref d 1)) (- (aref d 2))
                           (aref d 3)))))

(define-cproc quatf-conjugate! (q::<quatf> p::<quatf>)
  (let* ([s::float* (SCM_QUATF_D p)]
         [d::float* (SCM_QUATF_D q)])
    (SCM_QUATF_CONJUGATEV d s)
    (result (SCM_OBJ q))))

(define-cproc quatf-norm (q::<quatf>)
  (result (Scm_MakeFlonum (SCM_QUATF_NORMV (SCM_QUATF_D q)))))

(define-cproc quatf-normalize (q::<quatf>) Scm_QuatfNormalize)
(define-cproc quatf-normalize! (q::<quatf>) Scm_QuatfNormalizeX)

(define-cproc quatf->matrix4f (q::<quatf>)
  (let* ([m::(.array float [16])])
    (Scm_QuatfToMatrix4fv m (SCM_QUATF_D q))
    (result (Scm_MakeMatrix4fv m))))
(define-cproc quatf->matrix4f! (m::<matrix4f> q::<quatf>)
  (Scm_QuatfToMatrix4fv (SCM_MATRIX4F_D m) (SCM_QUATF_D q))
  (result (SCM_OBJ m)))

(define-cproc matrix4f->quatf (m::<matrix4f>)
  (let* ([q::(.array float [4])])
    (Scm_Matrix4fToQuatfv q (SCM_MATRIX4F_D m))
    (result (Scm_MakeQuatfv q))))

(define-cproc matrix4f->quatf! (q::<quatf> m::<matrix4f>)
  (Scm_Matrix4fToQuatfv (SCM_QUATF_D q) (SCM_MATRIX4F_D m))
  (result (SCM_OBJ q)))

(define-cproc quatf-slerp (p::<quatf> q::<quatf> t::<float>)
  (let* ([r::(.array float [4])])
    (Scm_QuatfSlerp r (SCM_QUATF_D p) (SCM_QUATF_D q) t)
    (result (Scm_MakeQuatfv r))))

(define-cproc quatf-slerp! (r::<quatf> p::<quatf> q::<quatf> t::<float>)
  (Scm_QuatfSlerp (SCM_QUATF_D r) (SCM_QUATF_D p) (SCM_QUATF_D q) t)
  (result (SCM_OBJ r)))

(define-cproc vectors->quatf (v::<vector4f> w::<vector4f>)
  (let* ([r::(.array float [4])])
    (Scm_VectorsToQuatfv r (SCM_VECTOR4F_D v) (SCM_VECTOR4F_D w))
    (result (Scm_MakeQuatfv r))))

(define-cproc vectors->quatf! (q::<quatf> v::<vector4f> w::<vector4f>)
  (Scm_VectorsToQuatfv (SCM_QUATF_D q) (SCM_VECTOR4F_D v) (SCM_VECTOR4F_D w))
  (result (SCM_OBJ q)))

(define-cproc axes->quatf (v1::<vector4f>
                           v2::<vector4f>
                           w1::<vector4f>
                           w2::<vector4f>)
  (let* ([r::(.array float [4])])
    (Scm_AxesToQuatfv r (SCM_VECTOR4F_D v1) (SCM_VECTOR4F_D v2)
                      (SCM_VECTOR4F_D w1) (SCM_VECTOR4F_D w2))
    (result (Scm_MakeQuatfv r))))

(define-cproc axes->quatf! (q::<quatf>
                                v1::<vector4f>
                                v2::<vector4f>
                                w1::<vector4f>
                                w2::<vector4f>)
  (Scm_AxesToQuatfv (SCM_QUATF_D q)
                    (SCM_VECTOR4F_D v1) (SCM_VECTOR4F_D v2)
                    (SCM_VECTOR4F_D w1) (SCM_VECTOR4F_D w2))
  (result (SCM_OBJ q)))

;; Local variables:
;; mode: scheme
;; end:
