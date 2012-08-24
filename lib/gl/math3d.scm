;;;
;;; gl/math3d.scm - auxiliary vector arithmetics for 3D graphics
;;;
;;;  Copyright (c) 2002-2012  Shiro Kawai  <shiro@acm.org>
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

(define-module gl.math3d
  (use gauche.uvector)
  (use gauche.sequence)
  (export-all)
  )

(select-module gl.math3d)

(dynamic-load "libgauche-math3d" :export-symbols #t)

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

(define (list->vector4f-array l)
  (let* ((len (length l))
         (a   (make-vector4f-array len))
         (i   0))
    (for-each (lambda (elt)
                (unless (vector4f? elt)
                  (error "vector4f required, but got" elt))
                (vector4f-array-set! a i elt)
                (inc! i))
              l)
    a))

(define (list->point4f-array l)
  (let* ((len (length l))
         (a   (make-point4f-array len))
         (i   0))
    (for-each (lambda (elt)
                (unless (point4f? elt)
                  (error "point4f required, but got" elt))
                (point4f-array-set! a i elt)
                (inc! i))
              l)
    a))

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
;; iterators
;;

;; Iterator common pattern.  We treat literal length separately, for it would
;; generate more efficient code.
(define-syntax %math3dobj-iterator
  (syntax-rules ()
    ((_ (size-expr . args) ref obj proc)
     (let ((len (size-expr . args))
           (i 0))
       (proc (lambda () (>= i len))
             (lambda () (begin0 (ref obj i) (inc! i))))))
    ((_ len ref obj proc)
     (let ((i 0))
       (proc (lambda () (>= i len))
             (lambda () (begin0 (ref obj i) (inc! i))))))
    ))

(define-method call-with-iterator ((v <vector4f>) proc . args)
  (%math3dobj-iterator 4 vector4f-ref v proc))

(define-method call-with-iterator ((p <point4f>) proc . args)
  (%math3dobj-iterator 4 point4f-ref p proc))

(define-method call-with-iterator ((m <matrix4f>) proc . args)
  (%math3dobj-iterator 16 matrix4f-ref m proc))

(define-method call-with-iterator ((q <quatf>) proc . args)
  (%math3dobj-iterator 4 quatf-ref q proc))

(define-method call-with-iterator ((a <vector4f-array>) proc . args)
  (%math3dobj-iterator (vector4f-array-length a) vector4f-array-ref a proc))

(define-method call-with-iterator ((a <point4f-array>) proc . args)
  (%math3dobj-iterator (point4f-array-length a) point4f-array-ref a proc))

;; Iterator builder pattern.
(define-syntax %math3dobj-builder
  (syntax-rules ()
    ((_ len new set proc)
     (let ((v (new)) (i 0))
       (proc (lambda (item) (when (>= i len) (set v i item) (inc! i)))
             (lambda () v))))))
     
(define-method call-with-builder ((class <vector4f-meta>) proc . args)
  (%math3dobj-builder 4 make-vector4f vector4f-set! proc))

(define-method call-with-builder ((class <point4f-meta>) proc . args)
  (%math3dobj-builder 4 make-point4f point4f-set! proc))

(define-method call-with-builder ((class <matrix4f-meta>) proc . args)
  (%math3dobj-builder 16 make-matrix4f matrix4f-set! proc))

(define-method call-with-builder ((class <quatf-meta>) proc . args)
  (%math3dobj-builder 4 make-quatf quatf-set! proc))

(define-syntax %math3dobj-builder*
  (syntax-rules ()
    ((_ new list-> set proc args)
     (let ((size (get-keyword :size args #f)))
       (if size
           (let ((v (new size)) (i 0))
             (proc (lambda (item)
                     (when (< i size) (set v i item) (inc! i)))
                   (lambda () v)))
           (let ((r '()))
             (proc (lambda (item) (push! r item))
                   (lambda () (list-> r)))))))))

(define-method call-with-builder ((class <vector4f-array-meta>) proc . args)
  (%math3dobj-builder* make-vector4f-array list->vector4f-array
                       vector4f-array-set! proc args))

(define-method call-with-builder ((class <point4f-array-meta>) proc . args)
  (%math3dobj-builder* make-point4f-array list->point4f-array
                       point4f-array-set! proc args))

;;-----------------------------------------------------------------
;; operator overload
;;
(define-method object-+ ((x <vector4f>) (y <vector4f>))
  (vector4f-add x y))
(define-method object-+ ((x <point4f>) (y <vector4f>))
  (point4f-add x y))
(define-method object-+ ((y <vector4f>) (x <point4f>))
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

(define-method object-* ((v <vector4f>) (f <real>))
  (vector4f-mul v f))
(define-method object-* ((f <real>) (v <vector4f>))
  (vector4f-mul v f))
(define-method object-/ ((v <vector4f>) (f <real>))
  (vector4f-div v f))

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
