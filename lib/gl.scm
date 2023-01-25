;;;
;;; gl.scm - Gauche GL binding
;;;
;;;  Copyright (c) 2001-2014  Shiro Kawai  <shiro@acm.org>
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

(define-module gl
  (use data.queue)
  (use gauche.sequence)
  (use gauche.uvector)
  (use gauche.version)
  (use scheme.list)
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
  (let ([len (gl-boolean-vector-length vec)]
        [i   (get-keyword :start args 0)])
    (proc (cut >= i len)
          (^[] (begin0 (gl-boolean-vector-ref vec i) (inc! i))))))

(define-method call-with-builder ((vec <gl-boolean-vector-meta>) proc . args)
  (if-let1 size (get-keyword :size args #f)
    (let ([v (make-gl-boolean-vector size)]
          [i 0])
      (proc (^[item]
              (when (< i size)
                (gl-boolean-vector-set! v i item)
                (inc! i)))
            (^[] v)))
    (let1 q (make-queue)
      (proc (cut enqueue! q <>)
            (cut list->gl-boolean-vector (dequeue-all! q))))))

(define-method size-of ((vec <gl-boolean-vector>))
  (gl-boolean-vector-length vec))

(define-reader-ctor 'gl-boolean-vector gl-boolean-vector)

;;-------------------------------------------------------------------
;; Utilities
;;

(define-syntax gl-begin*
  (syntax-rules ()
    [(_ mode commands ...)
     (begin
       (gl-begin mode) commands ... (gl-end))]
    ))

(define-syntax gl-push-matrix*
  (syntax-rules ()
    [(_ commands ...)
     (begin (gl-push-matrix) commands ... (gl-pop-matrix))]
    ))

;; Check GL version and extensions

(define-values
  (gl-extension-available?
   gl-version<? gl-version<=? gl-version>? gl-version>=? gl-version=?)
  (let ([gl-vers #f]
        [gl-exts #f])
    (define (ensure-version)
      (or gl-vers
          (rlet1 v (and-let* ([verstr (gl-get-string GL_VERSION)]
                              [m      (#/^\d+\.\d+\.\d+/ verstr)])
                     (m 0))
            (set! gl-vers v))))
    (define (ensure-extensions)
      (or gl-exts
          (rlet1 exts (and-let* ([extstr (gl-get-string GL_EXTENSIONS)])
                        (string-split extstr #[\s]))
            (set! gl-exts exts))))
    (define (gl-extension-available? . required)
      (and-let* ([exts (ensure-extensions)])
        (every (cut member <> exts)
               (map x->string required))))
    (define (gl-version<? v)
      (and-let* ([vers (ensure-version)]) (version<? vers v)))
    (define (gl-version<=? v)
      (and-let* ([vers (ensure-version)]) (version<=? vers v)))
    (define (gl-version>? v)
      (and-let* ([vers (ensure-version)]) (version>? vers v)))
    (define (gl-version>=? v)
      (and-let* ([vers (ensure-version)]) (version>=? vers v)))
    (define (gl-version=? v)
      (and-let* ([vers (ensure-version)]) (version=? vers v)))

    (values gl-extension-available?
            gl-version<? gl-version<=?
            gl-version>? gl-version>=?
            gl-version=?)
    ))
