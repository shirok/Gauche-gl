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
;;;  $Id: math3d.scm,v 1.8 2002-10-22 10:51:02 shirok Exp $
;;;

(define-module gl.math3d
  (use gauche.uvector)
  (use gauche.sequence)
  (export-all)
  )

(select-module gl.math3d)

(dynamic-load "gauche-math3d" :export-symbols #t)

(define-reader-ctor 'vector4f vector4f)
(define-reader-ctor 'point4f  point4f)
(define-reader-ctor 'matrix4f matrix4f)
(define-reader-ctor 'quatf    quatf)

(define-reader-ctor 'vector4f-array
  (lambda (length . vectors)
    (let1 arr (make-vector4f-array length)
      (let loop ((i 0)
                 (v vectors))
        (when (< i length)
          (vector4f-array-set! arr i (list->vector4f (car v)))
          (loop (+ i 1) (cdr v))))
      arr)))

(define-reader-ctor 'point4f-array
  (lambda (length . points)
    (let1 arr (make-point4f-array length)
      (let loop ((i 0)
                 (p points))
        (when (< i length)
          (point4f-array-set! arr i (list->point4f (car p)))
          (loop (+ i 1) (cdr p))))
      arr)))

;;=================================================================
;; Auxiliary fns
;;

(define (f32vector->vector4f-array v)
  (check-arg f32vector? v)
  (f32vector->vector4f-array/shared (f32vector-copy v)))

(define (f32vector->point4f-array v)
  (check-arg f32vector? v)
  (f32vector->point4f-array/shared (f32vector-copy v)))

;;=================================================================
;; sequence framework
;;

(define-method size-of ((x <vector4f>)) 4)
(define-method size-of ((x <point4f>)) 4)
(define-method size-of ((x <matrix4f>)) 16)
(define-method size-of ((x <quatf>)) 4)
(define-method size-of ((x <vector4f-array>)) (vector4f-array-length x))
(define-method size-of ((x <point4f-array>)) (point4f-array-length x))

;;-----------------------------------------------------------------
;; accessors and modifiers
;;
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

;;-----------------------------------------------------------------
;; conversions
;;
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

(define-method coerce-to ((c <list-meta>) (v <point4f>))
  (point4f->list v))
(define-method coerce-to ((c <f32vector-meta>) (v <point4f>))
  (point4f->f32vector v))
(define-method coerce-to ((c <vector-meta>) (v <point4f>))
  (list->vector (point4f->list v)))

(define-method coerce-to ((c <point4f-meta>) (v <list>))
  (list->point4f v))
(define-method coerce-to ((c <point4f-meta>) (v <f32vector>))
  (f32vector->point4f v))
(define-method coerce-to ((c <point4f-meta>) (v <vector>))
  (list->point4f (vector->list v)))

(define-method coerce-to ((c <list-meta>) (v <matrix4f>))
  (matrix4f->list v))
(define-method coerce-to ((c <f32vector-meta>) (v <matrix4f>))
  (matrix4f->f32vector v))
(define-method coerce-to ((c <vector-meta>) (v <matrix4f>))
  (list->vector (matrix4f->list v)))

(define-method coerce-to ((c <matrix4f-meta>) (v <list>))
  (list->matrix4f v))
(define-method coerce-to ((c <matrix4f-meta>) (v <f32vector>))
  (f32vector->matrix4f v))
(define-method coerce-to ((c <matrix4f-meta>) (v <vector>))
  (list->matrix4f (vector->list v)))

(define-method coerce-to ((c <list-meta>) (v <quatf>))
  (quatf->list v))
(define-method coerce-to ((c <f32vector-meta>) (v <quatf>))
  (quatf->f32vector v))
(define-method coerce-to ((c <vector-meta>) (v <quatf>))
  (list->vector (quatf->list v)))

(define-method coerce-to ((c <quatf-meta>) (v <list>))
  (list->quatf v))
(define-method coerce-to ((c <quatf-meta>) (v <f32vector>))
  (f32vector->quatf v))
(define-method coerce-to ((c <quatf-meta>) (v <vector>))
  (list->quatf (vector->list v)))

;;-----------------------------------------------------------------
;; operator overload
;;
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

(define-method object-- ((x <vector4f>))
  (vector4f-sub #,(vector4f 0 0 0) x))

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
