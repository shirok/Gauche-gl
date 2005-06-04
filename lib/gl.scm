;;;
;;; gl.scm - Gauche GL binding
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: gl.scm,v 1.7 2005-06-04 11:55:13 shirok Exp $
;;;

(define-module gl
  (use srfi-1)
  (use gauche.uvector)
  (use gauche.version)
  (extend gl.math3d)
  (export-all)
  )
(select-module gl)

(dynamic-load "libgauche-gl")

;; utilities

(define-syntax gl-begin*
  (syntax-rules ()
    ((_ mode commands ...)
     (begin
       (gl-begin mode) commands ... (gl-end)))
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
