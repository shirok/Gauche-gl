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
;;;  $Id: math3d.scm,v 1.3 2002-09-27 21:51:38 shirok Exp $
;;;

(define-module gl.math3d
  (use gauche.uvector)
  (use gauche.sequence)
  (export-all)
  )

(select-module gl.math3d)

(dynamic-load "gauche-math3d" :export-symbols #t)

(define-reader-ctor '<3dvector> 3dvector)
(define-reader-ctor '<3dpoint>  3dpoint)
(define-reader-ctor '<3dmatrix> 3dmatrix)
(define-reader-ctor '<quat>     quat)

;; collection framework
(define-method ref ((x <3dvector>) (i <integer>))
  (3dvector-ref x i))
(define-method ref ((x <3dpoint>) (i <integer>))
  (3dpoint-ref x i))
(define-method ref ((x <3dmatrix>) (i <integer>))
  (3dmatrix-ref x i))
(define-method ref ((x <quat>) (i <integer>))
  (quat-ref x i))
(define-method ref ((x <3dvector-array>) (i <integer>))
  (3dvector-array-ref x i))
(define-method ref ((x <3dpoint-array>) (i <integer>))
  (3dpoint-array-ref x i))


(define-method coerce-to ((c <list-meta>) (v <3dvector>))
  (3dvector->list v))
(define-method coerce-to ((c <f32vector-meta>) (v <3dvector>))
  (3dvector->f32vector v))
(define-method coerce-to ((c <vector-meta>) (v <3dvector>))
  (list->vector (3dvector->list v)))

(define-method coerce-to ((c <3dvector-meta>) (v <list>))
  (list->3dvector v))
(define-method coerce-to ((c <3dvector-meta>) (v <f32vector>))
  (f32vector->3dvector v))
(define-method coerce-to ((c <3dvector-meta>) (v <vector>))
  (list->3dvector (vector->list v)))

;; operator overload
(define-method object-+ ((x <3dvector>) (y <3dvector>))
  (3dvector-add x y))
(define-method object-+ ((x <3dpoint>) (y <3dvector>))
  (3dpoint-add x y))
(define-method object-+ ((x <quat>) (y <quat>))
  (quat-add x y))

(define-method object-- ((x <3dvector>) (y <3dvector>))
  (3dvector-sub x y))
(define-method object-- ((x <3dpoint>) (y <3dvector>))
  (3dpoint-sub x y))
(define-method object-- ((x <3dpoint>) (y <3dpoint>))
  (3dpoint-sub x y))
(define-method object-- ((x <quat>) (y <quat>))
  (quat-sub x y))

(define-method object-* ((m <3dmatrix>) (v <3dvector>))
  (3dmatrix-mul m v))
(define-method object-* ((m <3dmatrix>) (v <3dpoint>))
  (3dmatrix-mul m v))
(define-method object-* ((m <3dmatrix>) (n <3dmatrix>))
  (3dmatrix-mul m n))
(define-method object-* ((m <3dmatrix>) (s <real>))
  (3dmatrix-mul m s))
(define-method object-* ((s <real>) (m <3dmatrix>))
  (3dmatrix-mul m s))
(define-method object-* ((x <quat>) (y <quat>))
  (quat-mul x y))


(provide "gl/math3d")
