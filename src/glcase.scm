;;;
;;; A common cise macro to dispatch according to the argument type
;;;
;;;  Copyright (c) 2000-2012  Shiro Kawai  <shiro@acm.org>
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

;; This file is to be included by gl-lib.scm etc.

;; Quite a few Gauche-gl APIs are polymorphic, and we dispatch
;; to an appropriate GL APIs according to the argument type and
;; its size.  Hence we see lots of code that matches the following
;; pattern:
;;
;;   if (SCM_F32VECTORP(arg)) {
;;     switch (SCM_F32VECTOR_SIZE(arg)) {
;;       case 3: glFoo3fv((GLfloat*)SCM_F32VECTOR_ELEMENTS(arg)); break;
;;       case 4: glFoo4fv((GLfloat*)SCM_F32VECTOR_ELEMENTS(arg)); break;
;;       default: Scm_Error("blah blah blah, but got %S", arg)
;;     }
;;   } else if (SCM_F64VECTORP(arg)) {
;;     switch (SCM_F64VECTOR_SIZE(arg)) {
;;     ...
;;
;; (gl-case (v) "glFoo~n~v"
;;          "blah blah blah, but got %S"
;;          ((f32 3 4) (f64 3 4)))
;;
;;  ~n : replaced with the number of elements
;;  ~t : replaced with type suffix, e.g. 'i' or 'f'
;;  ~v : replaced with type suffix plus v, e.g. 'iv' or 'fv'

(define-cise-stmt gl-case
  [(_ (var ...) fname-fmt dispatches error-fmt)
   (define (type-pred tag)
     (string->symbol
      (ecase tag
        [(u8 s8 u16 s16 u32 s32 f32 f64)
         ($ format "SCM_~aVECTORP" $ string-upcase $ symbol->string tag)]
        [(m4f) "SCM_MATRIX4FP"]
        [(p4f) "SCM_POINT4FP"]
        [(v4f) "SCM_VECTOR4FP"]
        [(qf) "SCM_QUATFP"])))
   (define (do-cast tag expr)
     `(cast ,(string->symbol #`",(tag->ctype tag)*")
            ,(tag->accessor tag expr)))
   (define (tag->ctype tag)
     (ecase tag
       [(u8)  'GLubyte] [(s8) 'GLbyte] [(u16) 'GLushort] [(s16) 'GLshort]
       [(u32) 'GLuint] [(s32) 'GLint]
       [(f32 m4f p4f v4f qf) 'GLfloat] [(f64 args) 'GLdouble]))
   (define (tag->suffix tag)
     (ecase tag
       [(s8) "b"] [(u8) "ub"] [(s16) "s"] [(u16) "us"]
       [(s32) "i"] [(u32) "ui"] [(f32 m4f p4f v4f qf) "f"] [(f64 args) "d"]))
   (define (tag->enum tag)
     (ecase tag
       [(u8)  GL_UBYTE] [(s8) GL_BYTE] [(u16) GL_USHORT] [(s16) GL_SHORT]
       [(u32) GL_UINT] [(s32) GL_INT]
       [(f32 m4f p4f v4f qf) GL_FLOAT] [(f64 args) GL_DOUBLE]))
   (define (tag->accessor tag expr)
     (ecase tag
       [(u8 s8 u16 s16 u32 s32 f32 f64) `(SCM_UVECTOR_ELEMENTS ,expr)]
       [(m4f)     `(SCM_MATRIX4F_D ,expr)]
       [(p4f v4f) `(SCM_VECTOR4F_D ,expr)]
       [(qf)      `(SCM_QUATF_D ,expr)]
       [(args)     expr]))
   (define (gen-call tag nelems)
     (string->symbol
      (regexp-replace-all* fname-fmt
                           #/~n/ (^_ (if nelems (x->string nelems) ""))
                           #/~t/ (^_ (tag->suffix tag))
                           #/~v/ (^_ #`",(tag->suffix tag)v"))))
   (define (gen-dispatch-case tag nelems var)
     `[(,nelems) (,(gen-call tag nelems) ,(do-cast tag var))])
   (define (gen-dispatch-tag tag nelems-list)
     (match nelems-list
       [() `[(,(type-pred tag) ,(car var))
             (,(gen-call tag #f) ,(do-cast tag (car var)))]]
       [(nelems . nelems*)
        (case tag
          [(m4f p4f v4f qf)
          `[(,(type-pred tag) ,(car var))
            (,(gen-call tag nelems) ,(do-cast tag (car var)))]]
          [(args)
           (let ([val (gensym)] [nvals (gensym)])
             `[else
               (let* ([,val :: (.array double (4))]
                      [,nvals :: int
                              (Scm_GLGetDoubles ,@var ,val
                                                ,(apply max nelems-list)
                                                ,(apply min nelems-list))])
                 (case ,nvals
                   ,@(map (cut gen-dispatch-case tag <> val) nelems-list)
                   ))])]
          [else
           `[(,(type-pred tag) ,(car var))
             (case (SCM_UVECTOR_SIZE ,(car var))
               ,@(map (cut gen-dispatch-case tag <> (car var)) nelems-list)
               (else (Scm_Error ,error-fmt ,(car var))))]])]))
   `(cond
     ,@(map (^d (gen-dispatch-tag (car d) (cdr d))) dispatches)
     ,@(if (assq 'args dispatches)
         '()
         `((else (Scm_Error ,error-fmt ,(car var))))))])

       


