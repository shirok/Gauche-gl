;;;
;;; gl.scm - Gauche GL binding
;;;
;;;  Copyright (c) 2001-2008  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: gl.scm,v 1.11 2008-06-04 11:42:54 shirok Exp $
;;;

(define-module gl
  (use srfi-1)
  (use gauche.uvector)
  (use gauche.version)
  (use gauche.sequence)
  (extend gl.math3d)
  (export-all)
  )
(select-module gl)

(dynamic-load "libgauche-gl")

;;-------------------------------------------------------------------
;; <gl-boolean-vector> stuff
;;

(define-method ref ((vec <gl-boolean-vector>) k . maybe-default)
  (apply gl-boolean-vector-ref vec k maybe-default))

(define-method (setter ref) ((vec <gl-boolean-vector>) k value)
  (gl-boolean-vector-set! vec k value))

(define-method call-with-iterator ((vec <gl-boolean-vector>) proc . args)
  (let ((len (gl-boolean-vector-length vec))
        (i   (get-keyword :start args 0)))
    (proc (cut >= i len)
          (lambda () (begin0 (gl-boolean-vector-ref vec i) (inc! i))))))

(define-method call-with-builder ((vec <gl-boolean-vector-meta>) proc . args)
  (let ((size (get-keyword :size args #f)))
    (if size
      (let ((v (make-gl-boolean-vector size))
            (i 0))
        (proc (lambda (item)
                (when (< i size)
                  (gl-boolean-vector-set! v i item)
                  (inc! i)))
              (lambda () v)))
      (let ((q (make-queue)))
        (proc (cut enqueue! q <>)
              (cut list->gl-boolean-vector (dequeue-all! q)))))))

(define-method size-of ((vec <gl-boolean-vector>))
  (gl-boolean-vector-length vec))

(define-reader-ctor 'gl-boolean-vector gl-boolean-vector)

;;-------------------------------------------------------------------
;; Utilities
;;

(define-syntax gl-begin*
  (syntax-rules ()
    ((_ mode commands ...)
     (begin
       (gl-begin mode) commands ... (gl-end)))
    ))

(define-syntax gl-push-matrix*
  (syntax-rules ()
    ((_ commands ...)
     (begin (gl-push-matrix) commands ... (gl-pop-matrix)))
    ))

;; Check GL version and extensions

(define-values
  (gl-extension-available?
   gl-version<? gl-version<=? gl-version>? gl-version>=? gl-version=?)
  (let ((gl-vers #f)
        (gl-exts #f))
    (define (ensure-version)
      (or gl-vers
          (let1 v (and-let* ((verstr (gl-get-string GL_VERSION))
                            (m      (#/^\d+\.\d+\.\d+/ verstr)))
                    (m 0))
            (set! gl-vers v)
            v)))
    (define (ensure-extensions)
      (or gl-exts
          (let1 exts (and-let* ((extstr (gl-get-string GL_EXTENSIONS)))
                       (string-split extstr #[\s]))
            (set! gl-exts exts)
            exts)))
    (define (gl-extension-available? . required)
      (and-let* ((exts (ensure-extensions)))
        (every (cut member <> exts)
               (map x->string required))))
    (define (gl-version<? v)
      (and-let* ((vers (ensure-version))) (version<? vers v)))
    (define (gl-version<=? v)
      (and-let* ((vers (ensure-version))) (version<=? vers v)))
    (define (gl-version>? v)
      (and-let* ((vers (ensure-version))) (version>? vers v)))
    (define (gl-version>=? v)
      (and-let* ((vers (ensure-version))) (version>=? vers v)))
    (define (gl-version=? v)
      (and-let* ((vers (ensure-version))) (version=? vers v)))
    
    
    (values gl-extension-available?
            gl-version<? gl-version<=?
            gl-version>? gl-version>=?
            gl-version=?)
    ))

(provide "gl")
