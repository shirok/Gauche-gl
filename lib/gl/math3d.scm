;;;
;;; gl/math3d.scm - auxiliary vector arithmetics for 3D graphics
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: math3d.scm,v 1.6 2002-09-29 02:04:45 shirok Exp $
;;;

(define-module gl.math3d
  (use gauche.uvector)
  (use gauche.sequence)
  (export-all)
  )

(select-module gl.math3d)

(dynamic-load "gauche-math3d" :export-symbols #t)

(define-reader-ctor '<vector4f> vector4f)
(define-reader-ctor '<point4f>  point4f)
(define-reader-ctor '<matrix4f> matrix4f)
(define-reader-ctor '<quatf>    quatf)

;; Auxiliary fns

(define (f32vector->vector4f-array v)
  (check-arg f32vector? v)
  (f32vector->vector4f-array/shared (f32vector-copy v)))

(define (f32vector->point4f-array v)
  (check-arg f32vector? v)
  (f32vector->point4f-array/shared (f32vector-copy v)))

;; collection framework
(define-method ref ((x <vector4f>) (i <integer>))
  (vector4f-ref x i))
(define-method (setter ref) ((x <vector4f>) (i <integer>) v)
  (set! (vector4f-ref x i) v))

(define-method ref ((x <point4f>) (i <integer>))
  (point4f-ref x i))
(define-method (setter ref) ((x <point4f>) (i <integer>) v)
  (set! (point4f-ref x i) v))

(define-method ref ((x <matrix4f>) (i <integer>))
  (matrix4f-ref x i))
(define-method (setter ref) ((x <matrix4f>) (i <integer>) v)
  (set! (matrix4f-ref x i) v))

(define-method ref ((x <quatf>) (i <integer>))
  (quatf-ref x i))
(define-method (setter ref) ((x <quatf>) (i <integer>) v)
  (set! (quatf-ref x i) v))

(define-method ref ((x <vector4f-array>) (i <integer>))
  (vector4f-array-ref x i))
(define-method (setter ref) ((x <vector4f-array>) (i <integer>) v)
  (vector4f-array-set! x i v))

(define-method ref ((x <point4f-array>) (i <integer>))
  (point4f-array-ref x i))
(define-method (setter ref) ((x <point4f-array>) (i <integer>) v)
  (point4f-array-set! x i v))


(define-method coerce-to ((c <list-meta>) (v <vector4f>))
  (vector4f->list v))
(define-method coerce-to ((c <f32vector-meta>) (v <vector4f>))
  (vector4f->f32vector v))
(define-method coerce-to ((c <vector-meta>) (v <vector4f>))
  (list->vector (vector4f->list v)))

(define-method coerce-to ((c <vector4f-meta>) (v <list>))
  (list->vector4f v))
(define-method coerce-to ((c <vector4f-meta>) (v <f32vector>))
  (f32vector->vector4f v))
(define-method coerce-to ((c <vector4f-meta>) (v <vector>))
  (list->vector4f (vector->list v)))

;; operator overload
(define-method object-+ ((x <vector4f>) (y <vector4f>))
  (vector4f-add x y))
(define-method object-+ ((x <point4f>) (y <vector4f>))
  (point4f-add x y))
(define-method object-+ ((x <quatf>) (y <quatf>))
  (quatf-add x y))

(define-method object-- ((x <vector4f>) (y <vector4f>))
  (vector4f-sub x y))
(define-method object-- ((x <point4f>) (y <vector4f>))
  (point4f-sub x y))
(define-method object-- ((x <point4f>) (y <point4f>))
  (point4f-sub x y))
(define-method object-- ((x <quatf>) (y <quatf>))
  (quatf-sub x y))

(define-method object-* ((m <matrix4f>) (v <vector4f>))
  (matrix4f-mul m v))
(define-method object-* ((m <matrix4f>) (v <point4f>))
  (matrix4f-mul m v))
(define-method object-* ((m <matrix4f>) (n <matrix4f>))
  (matrix4f-mul m n))
(define-method object-* ((m <matrix4f>) (s <real>))
  (matrix4f-mul m s))
(define-method object-* ((s <real>) (m <matrix4f>))
  (matrix4f-mul m s))
(define-method object-* ((x <quatf>) (y <quatf>))
  (quatf-mul x y))


(provide "gl/math3d")
