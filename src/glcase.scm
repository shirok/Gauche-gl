;;;
;;; Common cise macros
;;;
;;;  Copyright (c) 2012-2014  Shiro Kawai  <shiro@acm.org>
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
;; The gl-case cise macro generates this pattern from a concise description,
;; as follows:
;;
;; (gl-case (v) "glFoo~n~v"
;;          ((f32 3 4) (f64 3 4)))
;;          "blah blah blah, but got %S"
;;
;; Here's the detailed spec:
;;
;; (gl-case <dispatch-variables>
;;          <call-pattern>
;;          <dispatches>
;;          <error-format-string>
;;          [<fallback-case>])
;;
;; <dispatch-variables> : (<variable>)
;;                      | (<variable> <restvar>)
;;
;;    This specifies which C variable gl-case should dispatch upon.
;;    The simplest case is a single-variable one, just the varaible
;;    we dispatch on.
;; 
;;    The second form is used for variable-arity api such as gl-vertex;
;;    it accepts vector argument, e.g. (gl-vertex #f32(1.0 1.0 1.0)), and
;;    individual real arguments, e.g. (gl-vertex 1.0 1.0 1.0).  If more than
;;    one argument are given, we gather the arguments as real numbers, and
;;    handles it according to 'args' dispatch (see belop).  Otherwise
;;    we dispatch on the type of first variable.
;;
;; <call-pattern> : <function-template>
;;                | (<function-template> <template> ...)
;;
;;    <function-template> is a string of the name of GL function.  It may
;;    contain sequences like ~n, which will be substituted according to the
;;    actual type of <variable>.   The following sequences are recognized:
;;      ~n : the number of elements, e.g. '3' in glVertex3f.
;;      ~t : the type suffix, e.g. 'f' in glVertex3f.
;;      ~v : the type suffix for vector argument, e.g. 'fv' in glVertex3fv.
;;
;;    In the second form you can write a cise form of calling the gl function.
;;    <template>s are cise expressions, but the following symbols are
;;    substituted according to the actual type of <variable>.
;;
;;      ~n : the number of elements, e.g. '3' in glVertex3f.
;;      ~E : the GL type enum, e.g. GL_FLOAT
;;      ~T : the C type, e.g. GLfloat
;;      ~X : the function/macro to extract the pointer to the actual data,
;;           e.g. SCM_F32VECTOR_ELEMENTS.
;;
;; <dispatches> : ((<type-tag> <count> ...) ...)
;; <type-tag> : u8 | s8 | u16 | s16 | u32 | s32 | f32 | f64
;;            | m4f | p4f | v4f | p4farray | v4farray | qf | args
;;
;;    u8 - f64  : uvector
;;    m4f : ScmMatrix4f
;;    p4f : ScmPoint4f
;;    v4f : ScmVector4f
;;    p4farray : ScmPoint4fArray
;;    v4farray : ScmVector4fArray
;;    qf  : ScmQuatf
;;    args : indicates to handle :rest type.  All args are coerced to
;;           doubles, put into an array of double and passed to gl*dv API.
;;
;; <count> : Specifies the # of elements in the <variable>.  m4f, p4f,
;;           v4f and qf have fixed size, but you can specify <count>
;;           if you want to give a different value to ~n.
;;
;; <fallback-case> : This code fragment is executed if the argument
;;           doesn't match any <type-tag>s, and no 'args' type tag
;;           is specified.

(define-cise-stmt gl-case
  [(_ (var ...) template dispatches error-fmt . maybe-fallback)
   (define (type-pred tag)
     (string->symbol
      (ecase tag
        [(u8 s8 u16 s16 u32 s32 f32 f64)
         ($ format "SCM_~aVECTORP" $ string-upcase $ symbol->string tag)]
        [(m4f) "SCM_MATRIX4FP"]
        [(p4f) "SCM_POINT4FP"]
        [(v4f) "SCM_VECTOR4FP"]
        [(p4farray) "SCM_POINT4F_ARRAY_P"]
        [(v4farray) "SCM_VECTOR4F_ARRAY_P"]
        [(qf) "SCM_QUATFP"])))
   (define (do-cast tag expr)
     `(cast ,(string->symbol #`",(tag->ctype tag)*")
            ,(tag->accessor tag expr)))
   (define (tag->ctype tag)
     (ecase tag
       [(u8)  'GLubyte] [(s8) 'GLbyte] [(u16) 'GLushort] [(s16) 'GLshort]
       [(u32) 'GLuint] [(s32) 'GLint]
       [(f32 m4f p4f v4f p4farray v4farray qf) 'GLfloat]
       [(f64 args) 'GLdouble]))
   (define (tag->suffix tag)
     (ecase tag
       [(s8) "b"] [(u8) "ub"] [(s16) "s"] [(u16) "us"]
       [(s32) "i"] [(u32) "ui"]
       [(f32 m4f p4f v4f p4farray v4farray qf) "f"] [(f64 args) "d"]))
   (define (tag->enum tag)
     (ecase tag
       [(u8)  'GL_UNSIGNED_BYTE] [(s8) 'GL_BYTE]
       [(u16) 'GL_UNSIGNED_SHORT] [(s16) 'GL_SHORT]
       [(u32) 'GL_UNSIGNED_INT] [(s32) 'GL_INT]
       [(f32 m4f p4f v4f p4farray v4farray qf) 'GL_FLOAT]
       [(f64 args) 'GL_DOUBLE]))
   (define (tag->accessor tag expr)
     (ecase tag
       [(u8 s8 u16 s16 u32 s32 f32 f64) `(SCM_UVECTOR_ELEMENTS ,expr)]
       [(m4f)     `(SCM_MATRIX4F_D ,expr)]
       [(p4f v4f) `(SCM_VECTOR4F_D ,expr)]
       [(p4farray) `(SCM_POINT4F_ARRAY_D ,expr)]
       [(v4farray) `(SCM_VECTOR4F_ARRAY_D ,expr)]
       [(qf)      `(SCM_QUATF_D ,expr)]
       [(args)     expr]))
   (define (gen-fname tag nelems fname-template)
     (string->symbol
      (regexp-replace-all* fname-template
                           #/~n/ (^_ (if nelems (x->string nelems) ""))
                           #/~t/ (^_ (tag->suffix tag))
                           #/~v/ (^_ #`",(tag->suffix tag)v"))))
   (define (subst-template s tag nelems var)
     (match s
       ['~n nelems]
       ['~E (tag->enum tag)]
       ['~T (tag->ctype tag)]
       ['~X (tag->accessor tag var)]
       [(a . b) (cons (subst-template a tag nelems var)
                      (subst-template b tag nelems var))]
       [(? string? fname) (gen-fname tag nelems fname)]
       [else s]))
   (define (gen-call tag nelems var)
     (match template
       [(? string? template)
        `(,(gen-fname tag nelems template) ,(do-cast tag var))]
       [exprs (subst-template exprs tag nelems var)]))
   (define (gen-dispatch-case tag nelems var)
     `[(,nelems) ,(gen-call tag nelems var)])
   (define (gen-error var)
     (if (string? error-fmt)
       `(Scm_Error ,error-fmt ,var)
       error-fmt))
   (define (gen-dispatch-tag tag nelems-list)
     (match nelems-list
       [() `[(,(type-pred tag) ,(car var))
             ,(gen-call tag (case tag
                              [(m4f) 16]
                              [(p4f v4f qf) 4]
                              [else #f])
                         (car var))]]
       [(nelems . nelems*)
        (case tag
          [(m4f p4f v4f p4farray v4farray qf)
           `[(,(type-pred tag) ,(car var))
             ,(gen-call tag nelems (car var))]]
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
               (else ,(gen-error (car var))))]])]))
   `(cond
     ,@(map (^d (gen-dispatch-tag (car d) (cdr d))) dispatches)
     ,@(cond [(assq 'args dispatches) '()]
             [(null? maybe-fallback)
              `((else ,(gen-error (car var))))]
             [else
              `((else ,(car maybe-fallback)))]))])
       
;; Other useful macros.

;; Check if var is a proper type of uvector with desired size

(define-cise-stmt assert-vector-type&size
  [(_ type size var)
   (let* ([TYPE (string-upcase (x->string type))]
          [pred (string->symbol #`"SCM_,|TYPE|VECTORP")]
          [getsize (string->symbol #`"SCM_,|TYPE|VECTOR_SIZE")]
          [msg #`",type of size ,size required,, but got %S"])
     `(when (or (not (,pred ,var)) (!= (,getsize ,var) ,size))
        (Scm_Error ,msg ,var)))])
