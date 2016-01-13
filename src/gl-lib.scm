;;;
;;; gl-lib.scm - glue functions for GL
;;;
;;;  Copyright (c) 2001-2015  Shiro Kawai  <shiro@acm.org>
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

(select-module gl)

(inline-stub

(declcode "#include \"gauche-gl.h\""
          "#include \"gl-syms.h\"")

(include "glcase.scm")

;; The functions are ordered in the same way as Mesa's header file.

"#define CHECK_ERROR(msg__)                            \\
  do {                                                 \\
    GLenum e = glGetError();                           \\
    if (e != GL_NO_ERROR) {                            \\
      Scm_Error(\"%s: %s\", msg__, gluErrorString(e)); \\
    }                                                  \\
  } while (0)
"

;; NB: this should be taken care of by genstub.
(define-type <uvector> "ScmUVector*" "uniform vector"
  "SCM_UVECTORP" "SCM_UVECTOR")
(define-type <u32vector> "ScmU32Vector*" "u32vector"
  "SCM_U32VECTORP" "SCM_U32VECTOR")
(define-type <f32vector> "ScmF32Vector*" "f32vector"
  "SCM_F32VECTORP" "SCM_F32VECTOR")
(define-type <gl-boolean-vector> "ScmGLBooleanVector*" "GL boolean vector"
  "SCM_GL_BOOLEAN_VECTOR_P" "SCM_GL_BOOLEAN_VECTOR")

;;=============================================================
;; <gl-boolean-vector> stuff
;;

(define-cproc list->gl-boolean-vector (lis) Scm_ListToGLBooleanVector)
(define-cproc gl-boolean-vector (&rest lis) Scm_ListToGLBooleanVector)
(define-cproc gl-boolean-vector? (obj) ::<boolean> SCM_GL_BOOLEAN_VECTOR_P)

(define-cproc make-gl-boolean-vector (size::<uint>
                                      &optional (init::<boolean> #f))
  Scm_MakeGLBooleanVector)

(define-cproc gl-boolean-vector-copy (bv::<gl-boolean-vector>)
  (result (Scm_MakeGLBooleanVectorFromArray (-> bv size) (-> bv elements))))

(define-cproc gl-boolean-vector-ref (bv::<gl-boolean-vector>
                                     k::<int>
                                     &optional fallback)
  (cond [(or (< k 0) (>= k (-> bv size)))
         (when (SCM_UNBOUNDP fallback)
           (Scm_Error "argument out of bound: %d" k))
         (result fallback)]
        [else
         (result (SCM_MAKE_BOOL (aref (-> bv elements) k)))]))

(define-cproc gl-boolean-vector-set! (bv::<gl-boolean-vector>
                                      k::<int>
                                      value)
  ::<void>
  (if (or (< k 0) (>= k (-> bv size)))
    (Scm_Error "argument out of bound: %d" k)
    (= (aref (-> bv elements) k) (?: (SCM_FALSEP value) GL_FALSE GL_TRUE))))

(define-cproc gl-boolean-vector-length (bv::<gl-boolean-vector>) ::<int>
  (result (-> bv size)))

(define-cproc gl-boolean-vector-fill! (bv::<gl-boolean-vector> fill)
  (let* ([val::GLboolean (?: (SCM_FALSEP fill) GL_FALSE GL_TRUE)])
    (dotimes [i (-> bv size)] (= (aref (-> bv elements) i) val)))
  (result (SCM_OBJ bv)))

;;=============================================================
;; Miscellaneous
;;

(define-cproc gl-clear-index (c::<float>) ::<void> glClearIndex)
(define-cproc gl-clear-color (r::<float> g::<float> b::<float> a::<float>)
  ::<void> glClearColor)
(define-cproc gl-clear (mask::<uint>) ::<void> glClear)
(define-cproc gl-index-mask (mask::<uint>) ::<void> glIndexMask)
(define-cproc gl-color-mask (r::<boolean> g::<boolean> b::<boolean> a::<boolean>)
  ::<void> glColorMask)

(define-cproc gl-alpha-func (func::<fixnum> ref::<float>)
  ::<void> glAlphaFunc)
(define-cproc gl-blend-func (sfactor::<fixnum> dfactor::<fixnum>)
  ::<void> glBlendFunc)
(define-cproc gl-logic-op (opcode::<fixnum>) ::<void> glLogicOp)
(define-cproc gl-cull-face (mode::<fixnum>) ::<void> glCullFace)
(define-cproc gl-front-face (mode::<fixnum>) ::<void> glFrontFace)
(define-cproc gl-point-size (size::<float>) ::<void> glPointSize)
(define-cproc gl-line-width (width::<float>) ::<void> glLineWidth)
(define-cproc gl-line-stipple (factor::<fixnum> pat::<ushort>)
  ::<void> glLineStipple)
(define-cproc gl-polygon-mode (face::<fixnum> mode::<fixnum>)
  ::<void> glPolygonMode)
(define-cproc gl-polygon-offset (factor::<float> units::<float>)
  ::<void> glPolygonOffset)

(define-cproc gl-polygon-stipple (mask) ::<void>
  (assert-vector-type&size u8 128 mask)
  (glPolygonStipple (SCM_U8VECTOR_ELEMENTS mask)))

(define-cproc gl-edge-flag (flag) ::<void>
  (if (SCM_GL_BOOLEAN_VECTOR_P flag)
    (glEdgeFlagv (-> (SCM_GL_BOOLEAN_VECTOR flag) elements))
    (glEdgeFlag (not (SCM_FALSEP flag)))))

(define-cproc gl-scissor (x::<int> y::<int> width::<int> height::<int>)
  ::<void> glScissor)

(define-cproc gl-clip-plane (plane::<fixnum> equation) ::<void>
  (assert-vector-type&size f64 4 equation)
  (glClipPlane plane (SCM_F64VECTOR_ELEMENTS equation)))

(define-cproc gl-get-clip-plane (plane::<fixnum>)
  (let* ([v::ScmF64Vector* (SCM_F64VECTOR (Scm_MakeF64Vector 4 0.0))])
    (glGetClipPlane plane (SCM_F64VECTOR_ELEMENTS v))
    (result (SCM_OBJ v))))

(define-cproc gl-draw-buffer (mode::<fixnum>) ::<void> glDrawBuffer)
(define-cproc gl-read-buffer (mode::<fixnum>) ::<void> glReadBuffer)

(define-cproc gl-enable (cap::<fixnum>) ::<void> glEnable)
(define-cproc gl-disable (cap::<fixnum>) ::<void> glDisable)
(define-cproc gl-is-enabled (cap::<fixnum>) ::<boolean> glIsEnabled)

(define-cproc gl-enable-client-state (cap::<fixnum>)
  ::<void> glEnableClientState)
(define-cproc gl-disable-client-state (cap::<fixnum>)
  ::<void> glDisableClientState)

;; Gauche-gl specific
(define-cproc gl-state-vector-size (state::<fixnum>)
  ::<int> Scm_GLStateInfoSize)

(define-cise-stmt with-state-info-size
  [(_ var state name . body)
   `(let* ([,var :: int (Scm_GLStateInfoSize ,state)])
      (when (<= ,var 0)
        (Scm_Error ,#`"you can't query state %x by ,name" ,state))
      ,@body)])

(define-cproc gl-get-boolean (state::<fixnum>)
  (with-state-info-size
   vsize state gl-get-boolean
   (if (== vsize 1)
     (let* ([b::GLboolean])
       (glGetBooleanv (cast GLenum state) (& b))
       (result (SCM_MAKE_BOOL b)))
     (let* ([v (Scm_MakeGLBooleanVector vsize GL_FALSE)])
       (glGetBooleanv (cast GLenum state)
                      (-> (SCM_GL_BOOLEAN_VECTOR v) elements))
       (result v)))))

(define-cproc gl-get-boolean! (vec::<gl-boolean-vector> state::<fixnum>)
  (with-state-info-size
   vsize state gl-get-boolean!
   (when (!= vsize (SCM_GL_BOOLEAN_VECTOR_SIZE vec))
      (Scm_Error "state %x needs a vector of size %d, but got %S"
                 state vsize (SCM_OBJ vec)))
   (glGetBooleanv (cast GLenum state) (-> vec elements))
   (result (SCM_OBJ vec))))

(define-cproc gl-get-integer (state::<fixnum>)
  (with-state-info-size
   vsize state gl-get-integer
   (if (== vsize 1)
     (let* ([i::GLint])
       (glGetIntegerv (cast GLenum state) (& i))
       (result (Scm_MakeInteger i)))
     (let* ([v::ScmS32Vector* (SCM_S32VECTOR (Scm_MakeS32Vector vsize 0))])
       (glGetIntegerv (cast GLenum state)
                      (cast GLint* (SCM_S32VECTOR_ELEMENTS v)))
       (result (SCM_OBJ v))))))

(define-cproc gl-get-integer! (vec::<s32vector> state::<fixnum>)
  (with-state-info-size
   vsize state gl-get-integer!
   (when (!= vsize (SCM_S32VECTOR_SIZE vec))
     (Scm_Error "state %x needs a vector of size %d, but got %S"
                state vsize vec))
   (glGetIntegerv (cast GLenum state)
                  (cast GLint* (SCM_S32VECTOR_ELEMENTS vec)))
   (result (SCM_OBJ vec))))

(define-cproc gl-get-float (state::<fixnum>)
  (with-state-info-size
   vsize state gl-get-float
   (if (== vsize 1)
     (let* ([v::GLfloat])
       (glGetFloatv (cast GLenum state) (& v))
       (result (Scm_MakeFlonum (cast double v))))
     (let* ([v::ScmF32Vector* (SCM_F32VECTOR (Scm_MakeF32Vector vsize 0))])
       (glGetFloatv (cast GLenum state) (SCM_F32VECTOR_ELEMENTS v))
       (result (SCM_OBJ v))))))

(define-cproc gl-get-float! (vec::<f32vector> state::<fixnum>)
  (with-state-info-size
   vsize state gl-get-float!
   (when (!= vsize (SCM_F32VECTOR_SIZE vec))
     (Scm_Error "state %x needs a vector of size %d, but got %S"
                state vsize vec))
   (glGetFloatv (cast GLenum state) (SCM_F32VECTOR_ELEMENTS vec))
   (result (SCM_OBJ vec))))

(define-cproc gl-get-double (state::<fixnum>)
  (with-state-info-size
   vsize state gl-get-double
   (if (== vsize 1)
     (let* ([v::GLdouble])
       (glGetDoublev (cast GLenum state) (& v))
       (result (Scm_MakeFlonum v)))
     (let* ([v::ScmF64Vector* (SCM_F64VECTOR (Scm_MakeF64Vector vsize 0))])
       (glGetDoublev (cast GLenum state) (SCM_F64VECTOR_ELEMENTS v))
       (result (SCM_OBJ v))))))

(define-cproc gl-get-double! (vec::<f64vector> state::<fixnum>)
  (with-state-info-size
   vsize state gl-get-double!
   (when (!= vsize (SCM_F64VECTOR_SIZE vec))
     (Scm_Error "state %x needs a vector of size %d, but got %S"
                state vsize vec))
   (glGetDoublev (cast GLenum state) (SCM_F64VECTOR_ELEMENTS vec))
   (result (SCM_OBJ vec))))

;; glGetPointerv

(define-cproc gl-push-attrib (mask::<ulong>)::<void> glPushAttrib)
(define-cproc gl-pop-attrib () ::<void> glPopAttrib)
(define-cproc gl-push-client-attrib (mask::<ulong>)::<void> glPushClientAttrib)
(define-cproc gl-pop-client-attrib () ::<void> glPopClientAttrib)

(define-cproc gl-render-mode (mode::<fixnum>) ::<int> glRenderMode)

(define-cproc gl-get-error () ::<int> glGetError)
(define-cproc gl-get-string (name::<fixnum>)
  (let* ([s::(const GLubyte*) (glGetString name)])
    (if s
      (result (Scm_MakeString (cast (const char*) s) -1 -1 SCM_MAKSTR_COPYING))
      (result SCM_FALSE))))

(define-cproc gl-flush () ::<void> glFlush)
(define-cproc gl-finish () ::<void> glFinish)
(define-cproc gl-hint (target::<int> mode::<int>) ::<void> glHint)

;;=============================================================
;; Depth Buffer
;;

(define-cproc gl-clear-depth (depth::<real>)::<void> glClearDepth)
(define-cproc gl-depth-func (func::<fixnum>)::<void> glDepthFunc)
(define-cproc gl-depth-mask (flag::<boolean>)::<void> glDepthMask)
(define-cproc gl-depth-range (nearv::<real> farv::<real>)::<void> glDepthRange)

;;=============================================================
;; Accumulation Buffer
;;

(define-cproc gl-clear-accum (r::<real> g::<real> b::<real> a::<real>)
  ::<void> glClearAccum)
(define-cproc gl-accum (op::<fixnum> value::<real>) ::<void> glAccum)

;;=============================================================
;; Transformation
;;

(define-cproc gl-matrix-mode (mode::<fixnum>) ::<void> glMatrixMode)

(define-cproc gl-ortho (left::<real> right::<real>
                        bottom::<real> top::<real>
                        nearv::<real> farv::<real>)
  ::<void> glOrtho)

(define-cproc gl-frustum (left::<real> right::<real>
                          bottom::<real> top::<real>
                          nearv::<real> farv::<real>)
  ::<void> glFrustum)

(define-cproc gl-viewport (x::<fixnum> y::<fixnum>
                           width::<fixnum> height::<fixnum>)
  ::<void> glViewport)

(define-cproc gl-push-matrix () ::<void> glPushMatrix)
(define-cproc gl-pop-matrix () ::<void> glPopMatrix)
(define-cproc gl-load-identity () ::<void> glLoadIdentity)

(define-cproc gl-load-matrix (m) ::<void>
  (gl-case (m) "glLoadMatrix~t"
           ((m4f) (f32 16) (f64 16))
           "matrix4f, or f32vector or f64vector of length 16 is required, but got %S"))

(define-cproc gl-mult-matrix (m) ::<void>
  (gl-case (m) "glMultMatrix~t"
           ((m4f) (f32 16) (f64 16))
           "matrix4f, or f32vector or f64vector of length 16 is required, but got %S"))

(define-cproc gl-rotate (angle::<real> x::<real> y::<real> z::<real>)
  ::<void> glRotated)
(define-cproc gl-scale (x::<real> y::<real> z::<real>)
  ::<void> glScaled)
(define-cproc gl-translate (x::<real> y::<real> z::<real>)
  ::<void> glTranslated)

;;=============================================================
;; Display lists
;;

(define-cproc gl-is-list (list::<int>)::<boolean> glIsList)
(define-cproc gl-delete-lists (list::<int> range::<int>) ::<void> glDeleteLists)
(define-cproc gl-gen-lists (range::<int>) ::<ulong> glGenLists)
(define-cproc gl-new-list (list::<int> mode::<int>) ::<void> glNewList)
(define-cproc gl-end-list () ::<void> glEndList)
(define-cproc gl-call-list (list::<int>) ::<void> glCallList)

;; this may be called as
;;  (gl-call-lists array)
;;  (gl-call-lists size array)
;;  (gl-call-lists size type array)
(define-cproc gl-call-lists (arg0 &optional arg1 arg2) ::<void>
  (let* ([size::GLsizei 0] [type::int -1] [array] [bad])
    (if (SCM_UNBOUNDP arg2)
      (if (SCM_UNBOUNDP arg1)
        (set! array arg0)               ; 1arg
        (if (not (SCM_INTP arg0))
          (begin (set! bad arg0) (goto badarg))
          (begin (set! size (SCM_INT_VALUE arg0)) (set! array arg1)))) ; 2args
      (begin                            ; 3args
        (unless (SCM_INTP arg0) (set! bad arg0) (goto badarg))
        (set! size (SCM_INT_VALUE arg0))
        (unless (SCM_INTP arg1) (set! bad arg1) (goto badarg))
        (set! type (SCM_INT_VALUE arg1))
        (set! array arg2)))

    (cond [(SCM_U8VECTORP array)
           (case type
             [(-1)
              (if (== size 0)
                (set! size (SCM_U8VECTOR_SIZE array))
                (when (< (SCM_U8VECTOR_SIZE array) size) (goto nee)))
              (glCallLists size GL_UNSIGNED_BYTE (SCM_U8VECTOR_ELEMENTS array))]
             [(GL_2_BYTES)
              (if (== size 0)
                (set! size (/ (SCM_U8VECTOR_SIZE array) 2))
                (when (< (/ (SCM_U8VECTOR_SIZE array) 2) size) (goto nee)))
              (glCallLists size GL_2_BYTES (SCM_U8VECTOR_ELEMENTS array))]
             [(GL_3_BYTES)
              (if (== size 0)
                (set! size (/ (SCM_U8VECTOR_SIZE array) 3))
                (when (< (/ (SCM_U8VECTOR_SIZE array) 3) size) (goto nee)))
              (glCallLists size GL_3_BYTES (SCM_U8VECTOR_ELEMENTS array))]
             [(GL_4_BYTES)
              (if (== size 0)
                (set! size (/ (SCM_U8VECTOR_SIZE array) 4))
                (when (< (/ (SCM_U8VECTOR_SIZE array) 4) size) (goto nee)))
              (glCallLists size GL_4_BYTES (SCM_U8VECTOR_ELEMENTS array))]
             [else (goto badtype)])]
          [(SCM_STRINGP array)
           ;; TODO: for MT safety, we should extract stringbody first
           (if (== size 0)
             (set! size (SCM_STRING_SIZE array))
             (when (< (SCM_STRING_SIZE array) size) (goto nee)))
           (glCallLists size GL_UNSIGNED_BYTE (SCM_STRING_START array))]
          [else
           (gl-case (array)
                    (begin
                      (if (== size 0)
                        (set! size (SCM_UVECTOR_SIZE array))
                        (when (< (SCM_UVECTOR_SIZE array) size) (goto nee)))
                      (glCallLists size ~E ~X))
                    ((s8) (s16) (u16) (s32) (u32) (f32))
                    "bad argument to gl-call-lists: %S")])
    (when 0
      (label badarg)
      (Scm_Error "bad argument to gl-call-lists: %S" bad))
    (when 0
      (label nee)
      (Scm_Error "not enough elements passed to gl-call-lists: %S" array))
    (when 0
      (label badtype)
      (Scm_Error "given type %d doesn't match the passed array (u8vector)"
                 type))
    ))


(define-cproc gl-list-base (base::<int>) ::<void> glListBase)

;;=============================================================
;; Drawing functions
;;

(define-cproc gl-begin (mode::<int>) ::<void> glBegin)
(define-cproc gl-end () ::<void> glEnd)

(define-cproc gl-vertex (v &rest args)
  (gl-case (v args) "glVertex~n~v"
           ((p4f 3) (v4f 3) (f32 3 2 4) (f64 3 2 4) (s32 3 2 4) (s16 3 2 4)
            (args 3 2 4))
           "bad argument for v: %S, must be one of point4f, vector4f, \
            or f32, f64, s32 or s16 vector of length 2, 3, or 4."))

(define-cproc gl-normal (v &rest args)
  (gl-case (v args) "glNormal3~v"
           ((v4f) (f32 3) (f64 3) (s32 3) (s16 3) (args 3))
           "bad argument for v: %S, must be one of vector4f, \
            or f32, f64, s32 or s16 vector of length 3."))

(define-cproc gl-index (v) ::<void>
  (cond
   [(SCM_REALP v)      (glIndexd (Scm_GetDouble v))]
   [(and (SCM_UVECTORP v) (>= (SCM_UVECTOR_SIZE v) 1))
    (gl-case (v) "glIndex~v"
             ((s16) (s32) (f32) (f64))
             "s16, s32, f32, f64 or u8vector is required, but got %S")]
   [else
    (SCM_TYPE_ERROR v "real number or s16, s32, f32, f64 or u8vector of at least one element")]))

(define-cproc gl-color (v &rest args) ::<void>
  (gl-case (v args) "glColor~n~v"
           ((f32 3 4) (f64 3 4) (u8 3 4) (u16 3 4) (u32 3 4)
            (s8 3 4) (s16 3 4) (s32 3 4) (args 3 4))
           "bad argument for color: %S, must be an uniform vector of \
            length 3 or 4"))
            

(define-cproc gl-tex-coord (v &rest args) ::<void>
  (gl-case (v args) "glTexCoord~n~v"
           ((f32 2 1 3 4) (f64 2 1 3 4) (s32 2 1 3 4) (s16 2 1 3 4)
            (args 2 1 3 4))
           "bad argument for v: %S, must be one of f32, f64, s32 or s16 vector of length 1, 2, 3, or 4."))

(define-cproc gl-raster-pos (v &rest args)
  (gl-case (v args) "glRasterPos~n~v"
           ((f32 3 2 4) (f64 3 2 4) (s32 3 2 4) (s16 3 2 4) (args 3 2 4))
           "bad argument for v: %S, must be one of f32, f64, s32 or s16 vector of length 2, 3, or 4."))

(define-cproc gl-rect (v1 v2 &rest args)
  (cond
   [(SCM_POINT4FP v1)
    (unless (SCM_POINT4FP v2) (goto badarg2))
    (glRectfv (SCM_POINT4F_D v1) (SCM_POINT4F_D v2))]
   [(SCM_F32VECTORP v1)
    (unless (== (SCM_F32VECTOR_SIZE v1) 2) (goto badarg1))
    (unless (and (SCM_F32VECTORP v2) (== (SCM_F32VECTOR_SIZE v2) 2))
      (goto badarg2))
    (glRectfv (SCM_F32VECTOR_ELEMENTS v1) (SCM_F32VECTOR_ELEMENTS v2))]
   [(SCM_F64VECTORP v1)
    (unless (== (SCM_F64VECTOR_SIZE v1) 2) (goto badarg1))
    (unless (and (SCM_F64VECTORP v2) (== (SCM_F64VECTOR_SIZE v2) 2))
      (goto badarg2))
    (glRectdv (SCM_F64VECTOR_ELEMENTS v1) (SCM_F64VECTOR_ELEMENTS v2))]
   [(SCM_S32VECTORP v1)
    (unless (== (SCM_S32VECTOR_SIZE v1) 2) (goto badarg1))
    (unless (and (SCM_S32VECTORP v2) (== (SCM_S32VECTOR_SIZE v2) 2))
      (goto badarg2))
    (glRectiv (SCM_S32VECTOR_ELEMENTS v1) (SCM_S32VECTOR_ELEMENTS v2))]
   [(SCM_S16VECTORP v1)
    (unless (== (SCM_S16VECTOR_SIZE v1) 2) (goto badarg1))
    (unless (and (SCM_S16VECTORP v2) (== (SCM_S16VECTOR_SIZE v2) 2))
      (goto badarg2))
    (glRectsv (SCM_S16VECTOR_ELEMENTS v1) (SCM_S16VECTOR_ELEMENTS v2))]
   [else
    (let* ([val::(.array double (4))])
      (Scm_GLGetDoubles v1 (Scm_Cons v2 args) val 4 4)
      (glRectd (aref val 0) (aref val 1) (aref val 2) (aref val 3)))])
  (when 0
    (label badarg1)
    (Scm_Error "bad argument for v1: %S, must be one of f32, f64, s32 or s16 vector of length 2" v1))
  (when 0
    (label badarg2)
    (Scm_Error "bad argument for v2: %S, must be one of f32, f64, s32 or s16 vector of length 2" v2)))

;;=============================================================
;; Vertex Arrays
;;

;; NOTE: it is caller's responsibility to guarantee VEC has enough length.
;; GL doesn't have interface to specify the boundary, so I can't detect
;; invalid length vector.

;; Scheme version doesn't have TYPE - it's derived from vector type.
;; STRIDE argument refers to the # of elements, rather than bytes.

(define-cproc gl-vertex-pointer (size::<fixnum> vec
                                 &optional (stride::<fixnum> 0)
                                           (offset::<fixnum> 0))
  ::<void>
  (when (or (< size 2) (> size 4))
    (Scm_Error "bad argument for size: %d, must be 2, 3 or 4" size))
  (when (< stride 0)
    (Scm_Error "bad argument for stride: %d, must be 0 or positive" stride))
  (when (< offset 0)
    (Scm_Error "bad argument for offset: %d, must be 0 or positive" offset))
  (gl-case (vec)
           ("glVertexPointer" size ~E (* stride (sizeof ~T))
            (cast void* (+ ~X offset)))
           ((p4farray) (f32) (f64) (s32) (s16))
           "bad argument for vec: %S, must be f32, f64, s32 or s16vector"))

(define-cproc gl-normal-pointer (vec
                                 &optional (stride::<fixnum> 0)
                                           (offset::<fixnum> 0))
  ::<void>
  (when (< stride 0)
    (Scm_Error "bad argument for stride: %d, must be 0 or positive" stride))
  (when (< offset 0)
    (Scm_Error "bad argument for offset: %d, must be 0 or positive" offset))
  (gl-case (vec)
           ("glNormalPointer" ~E (* stride (sizeof ~T))
            (cast void* (+ ~X offset)))
           ((v4farray) (f32) (f64) (s32) (s16) (s8))
           "bad argument for vec: %S, must be f32, f64, s8, s16 or s32vector"))

(define-cproc gl-color-pointer (size::<fixnum> vec
                                &optional (stride::<fixnum> 0)
                                          (offset::<fixnum> 0))
  ::<void>
  (when (or (< size 2) (> size 4))
    (Scm_Error "bad argument for size: %d, must be 2, 3 or 4" size))
  (when (< stride 0)
    (Scm_Error "bad argument for stride: %d, must be 0 or positive" stride))
  (when (< offset 0)
    (Scm_Error "bad argument for offset: %d, must be 0 or positive" offset))
  (gl-case (vec)
           ("glColorPointer" size ~E (* stride (sizeof ~T))
            (cast void* (+ ~X offset)))
           ((f32) (f64) (u32) (u16) (u8) (s32) (s16) (s8))
           "bad argument for vec: %S, must be f32, f64, s8, u8, s16, u16, s32 or u32vector"))

(define-cproc gl-index-pointer (vec
                                &optional (stride::<fixnum> 0)
                                          (offset::<fixnum> 0))
  ::<void>
  (when (< stride 0)
    (Scm_Error "bad argument for stride: %d, must be 0 or positive" stride))
  (when (< offset 0)
    (Scm_Error "bad argument for offset: %d, must be 0 or positive" offset))
  (gl-case (vec)
           ("glIndexPointer" ~E (* stride (sizeof ~T))
            (cast void* (+ ~X offset)))
           ((s32) (s16) (u8) (f32) (f64))
           "bad argument for vec: %S, must be f32, f64, u8, s16 or s32 vector"))

(define-cproc gl-tex-coord-pointer (size::<fixnum> vec
                                    &optional (stride::<fixnum> 0)
                                              (offset::<fixnum> 0))
  ::<void>
  (when (or (< size 1) (> size 4))
    (Scm_Error "bad argument for size: %d, must be 1, 2, 3 or 4" size))
  (when (< stride 0)
    (Scm_Error "bad argument for stride: %d, must be 0 or positive" stride))
  (when (< offset 0)
    (Scm_Error "bad argument for offset: %d, must be 0 or positive" offset))
  (gl-case (vec)
           ("glTexCoordPointer" size ~E (* stride (sizeof ~T))
            (cast void* (+ ~X offset)))
           ((f32) (f64) (s32) (s16))
           "bad argument for vec: %S, must be f32, f64, s16 or s32vector"))

(define-cproc gl-edge-flag-pointer (vec
                                    &optional (stride::<fixnum> 0)
                                              (offset::<fixnum> 0))
  ::<void>
  (if (SCM_GL_BOOLEAN_VECTOR_P vec)
    (glEdgeFlagPointer stride (+ (SCM_GL_BOOLEAN_VECTOR_ELEMENTS vec) offset))
    (Scm_Error "gl-boolean-vector required for vec, but got %S" vec)))

(define-cproc gl-array-element (ith::<fixnum>) ::<void>
  (glArrayElement ith))

;; count and type is derived from indices vector
(define-cproc gl-draw-elements (mode::<fixnum> indices) ::<void>
  (gl-case (indices)
           ("glDrawElements" mode (SCM_UVECTOR_SIZE indices) ~E ~X)
           ((u8) (u16) (u32))
           "bad argument for indices: %S, must be u8, u16 or u32vector"))

(define-cproc gl-draw-arrays (mode::<fixnum> first::<fixnum> count::<fixnum>)
  ::<void> glDrawArrays)

;; Note: we don't allow non-uniform vector for the interleaved arrays, so
;; the color component must be float.
(define-cproc gl-interleaved-arrays (format::<fixnum> vec
                                     &optional (stride::<fixnum> 0)
                                               (offset::<fixnum> 0))
  ::<void>
  (case format
    [(GL_C4UB_V2F GL_C4UB_V3F GL_T2F_C4UB_V3F)
     (Scm_Error "interleaved arrays with integer color component is not supported.")])
  (unless (SCM_F32VECTORP vec)
    (Scm_Error "bad argument for vec: %S, must be f32vector." vec))
  (glInterleavedArrays format (* stride (sizeof GLfloat))
                       (SCM_F32VECTOR_ELEMENTS vec)))

;;=============================================================
;; Lighting
;;

(define-cproc gl-shade-model (mode::<fixnum>) ::<void> glShadeModel)

(define-cproc gl-light (light::<fixnum> pname::<fixnum> param) ::<void>
  (case pname
    [(GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION)
     (gl-case (param) ("glLight~v" light pname ~X) ((f32 4) (s32 4))
              "f32 or s32 vector of length 4 expected, but got %S")]
    [(GL_SPOT_DIRECTION)
     (gl-case (param) ("glLight~v" light pname ~X) ((f32 3) (s32 3))
              "f32 or s32 vector of length 3 expected, but got %S")]
    [else
     (if (SCM_REALP param)
       (glLightf light pname (cast GLfloat (Scm_GetDouble param)))
       (Scm_Error "real number required, but got %S" param))]))

(define-cproc gl-get-light (light::<fixnum> pname::<fixnum>)
  (case pname
    [(GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION)
     (let* ([v (Scm_MakeF32Vector 4 0.0)])
       (glGetLightfv light pname (SCM_F32VECTOR_ELEMENTS v))
       (result v))]
    [(GL_SPOT_DIRECTION)
     (let* ([v (Scm_MakeF32Vector 3 0.0)])
       (glGetLightfv light pname (SCM_F32VECTOR_ELEMENTS v))
       (result v))]
    [else
     (let* ([f::GLfloat])
       (glGetLightfv light pname (& f))
       (result (Scm_MakeFlonum (cast double f))))]))

(define-cproc gl-light-model (pname::<fixnum> param) ::<void>
  (case pname
    [(GL_LIGHT_MODEL_AMBIENT)
     (gl-case (param) ("glLightModel~v" pname ~X) ((f32 4) (s32 4))
              "bad parameter for GL_LIGHT_MODEL_AMBIENT: f32 or s32 vector of length 4 is expected, but got %S")]
    [(GL_LIGHT_MODEL_COLOR_CONTROL)
     (if (SCM_INTP param)
       (glLightModeli pname (SCM_INT_VALUE param))
       (Scm_Error "bad parameter for GL_LIGHT_MODEL_COLOR_CONTROL: an exact small integer is expected, but got %S" param))]
    [else (glLightModeli pname (not (SCM_FALSEP param)))]))

(define-cproc gl-material (face::<fixnum> pname::<fixnum> param) ::<void>
  (case pname
    [(GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_EMISSION GL_AMBIENT_AND_DIFFUSE)
     (gl-case (param) ("glMaterial~v" face pname ~X) ((f32 4) (s32 4))
              "f32 or s32 vector of length 4 expected, but got %S")]
    [(GL_COLOR_INDEXES)
     (gl-case (param) ("glMaterial~v" face pname ~X) ((f32 3) (s32 3))
              "f32 or s32 vector of length 3 expected, but got %S")]
    [else
     (if (SCM_REALP param)
       (glMaterialf face pname (cast GLfloat (Scm_GetDouble param)))
       (Scm_Error "real number expected, but got %S" param))]))

(define-cproc gl-get-material (face::<fixnum> pname::<fixnum>)
  (case pname
    [(GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_EMISSION)
     (let* ([v (Scm_MakeF32Vector 4 0.0)])
       (glGetMaterialfv face pname (SCM_F32VECTOR_ELEMENTS v))
       (result v))]
    [(GL_COLOR_INDEXES)
     (let* ([v (Scm_MakeS32Vector 4 0.0)])
       (glGetMaterialiv face pname (SCM_S32VECTOR_ELEMENTS v))
       (result v))]
    [(GL_SHININESS)
     (let* ([v::GLfloat])
       (glGetMaterialfv face pname (& v))
       (result (Scm_MakeFlonum (cast double v))))]
    [else (Scm_Error "bad pname: %d" pname)]))

(define-cproc gl-color-material (face::<fixnum> mode::<fixnum>)
  ::<void> glColorMaterial)

;;=============================================================
;; Raster functions
;;

(define-cproc gl-pixel-zoom (xfactor::<real> yfactor::<real>)
  ::<void> glPixelZoom)

(define-cproc gl-pixel-store (pname::<fixnum> param) ::<void>
  (cond [(SCM_INTP param) (glPixelStorei pname (Scm_GetInteger param))]
        [(SCM_REALP param)
         (glPixelStoref pname (cast GLfloat (Scm_GetDouble param)))]
        [else (Scm_Error "real number required for param, but got %S" param)]))

(define-cproc gl-pixel-transfer (pname::<fixnum> param) ::<void>
  (cond [(SCM_BOOLP param) (glPixelTransferi pname (SCM_BOOL_VALUE param))]
        [(SCM_INTP param) (glPixelTransferi pname (Scm_GetInteger param))]
        [(SCM_REALP param)
         (glPixelTransferf pname (cast GLfloat (Scm_GetDouble param)))]
        [else (Scm_Error "real number or boolean required for param, but got %S" param)]))

(define-cproc gl-pixel-map (map::<fixnum> values) ::<void>
  (gl-case (values) ("glPixelMap~v" map (SCM_UVECTOR_SIZE values) ~X)
           ((u32) (u16) (f32))
           "map value vector must be u16, u32 or f32 vector, but got %S"))

;; values must have enough size
(define-cproc gl-get-pixel-map! (map::<fixnum> values)
  (gl-case (values) ("glGetPixelMap~v" map ~X) ((u32) (u16) (f32))
           "map value vector must be u16, u32 or f32 vector, but got %S")
  (result values))

;; allocate the vector in it.  type can be a class 
;; <u32vector> (default), <u16vector> or <f32vector>
(define-cproc gl-get-pixel-map (map::<fixnum> &optional type)
  (let* ([size::int])
    (glGetIntegerv map (& size))
    (CHECK_ERROR "couldn't get pixel map size")
    (SCM_ASSERT (>= size 0))
    (cond
     [(or (SCM_UNBOUNDP type) (== type (SCM_OBJ SCM_CLASS_U32VECTOR)))
      (let* ([vec (Scm_MakeU32Vector size 0)])
        (glGetPixelMapuiv map (cast GLuint* (SCM_U32VECTOR_ELEMENTS vec)))
        (result vec))]
     [(== type (SCM_OBJ SCM_CLASS_U16VECTOR))
      (let* ([vec (Scm_MakeU16Vector size 0)])
        (glGetPixelMapusv map (cast GLushort* (SCM_U16VECTOR_ELEMENTS vec)))
        (result vec))]
     [(== type (SCM_OBJ SCM_CLASS_F32VECTOR))
      (let* ([vec (Scm_MakeF32Vector size 0)])
        (glGetPixelMapfv map (cast GLfloat* (SCM_F32VECTOR_ELEMENTS vec)))
        (result vec))]
     [else
      (Scm_Error "pixel map vector class must be either <u32vector>, <u16vector> or <f32vector>, but got %S" type)])))

(define-cproc gl-bitmap (width::<int> height::<int>
                         xbo::<float> ybo::<float>
                         xbi::<float> ybi::<float> bitmap)
  ::<void>
  (unless (SCM_U8VECTORP bitmap)
    (Scm_Error "bitmap must be an u8vector, but got %S" bitmap))
  (unless (== (SCM_U8VECTOR_SIZE bitmap) (* height (/ (+ width 7) 8)))
    (Scm_Error "size (width=%d, height=%d) doesn't match bitmap vector %S"
               width height bitmap))
  (glBitmap width height xbo ybo xbi ybi (SCM_U8VECTOR_ELEMENTS bitmap)))

(define-cproc gl-read-pixels (x::<fixnum> y::<fixnum>
                              width::<fixnum> height::<fixnum>
                              format::<fixnum> type::<fixnum>)
  (let* ([elttype::int] [packed::int]
         [size::int (Scm_GLPixelDataSize width height format type
                                         (& elttype) (& packed))]
         [vec (Scm_GLAllocUVector elttype size)])
    (unless (SCM_UVECTORP vec)
      (Scm_Error "invalid format or type (%S, %S)" format type))
    (glReadPixels x y width height format type (SCM_UVECTOR_ELEMENTS vec))
    (result vec)))

; gl-read-pixels!

(define-cproc gl-draw-pixels (width::<fixnum> height::<fixnum>
                              format::<fixnum> type::<fixnum> pixels)
  ::<void>
  (let* ([elttype::int] [packed::int]
         [size::int (Scm_GLPixelDataSize width height format type
                                         (& elttype) (& packed))])
    (glDrawPixels width height format type
                  (Scm_GLPixelDataCheck pixels elttype size))))

(define-cproc gl-copy-pixels (x::<fixnum> y::<fixnum>
                              width::<fixnum> height::<fixnum> type::<fixnum>)
  ::<void> glCopyPixels)

;;=============================================================
;; Stenciling
;;

(define-cproc gl-stencil-func (func::<fixnum> ref::<int> mask::<uint>)
  ::<void> glStencilFunc)
(define-cproc gl-stencil-mask (mask::<uint>)
  ::<void> glStencilMask)
(define-cproc gl-stencil-op (func::<fixnum> zfail::<fixnum> zpass::<fixnum>)
  ::<void> glStencilOp)
(define-cproc gl-clear-stencil (s::<int>)
  ::<void> glClearStencil)

;;=============================================================
;; Texture mapping
;;

(define-cproc gl-tex-gen (coord::<fixnum> pname::<fixnum> param) ::<void>
  (case pname
    [(GL_TEXTURE_GEN_MODE)
     (unless (SCM_INTP param)
       (Scm_Error "integer parameter required for GL_TEXTURE_GEN_MODE, but got %S" param))
     (glTexGeni coord pname (SCM_INT_VALUE param))]
    [(GL_OBJECT_PLANE GL_EYE_PLANE)
     (gl-case (param) ("glTexGen~v" coord pname ~X)
              ((f32 4) (f64 4) (s32 4))
              "s32, f32 or f64 vector of size 4 is required for parameter, but got %S")]
    [else (Scm_Error "unknown or unsupported glTexGen pname: %d" pname)]))

(define-cproc gl-tex-env (target::<fixnum> pname::<fixnum> param) ::<void>
  (case pname
    [(GL_TEXTURE_ENV_MODE)
     (if (SCM_INTP param)
       (glTexEnvi target pname (SCM_INT_VALUE param))
       (Scm_Error "integer parameter required for GL_TEXTURE_ENV_MODE, but got %S" param))]
    [(GL_TEXTURE_ENV_COLOR)
     (gl-case (param) ("glTexEnv~v" target pname ~X) ((f32 4))
              "f32 vector of size 4 is required for GL_TEXTURE_ENV_COLOR parameter, but got %S")]
    [else (Scm_Error "unknown or unsupported glTexEnv pname: %d" pname)]))

;; Trick: CYGWIN GL missing some parameters.  We define dummy here---even
;; with correct values, they'll fail at runtime anyway.
"#ifndef GL_TEXTURE_WRAP_R
#define GL_TEXTURE_WRAP_R 0
#endif
#ifndef GL_TEXTURE_WRAP_BASE_LEVEL
#define GL_TEXTURE_WRAP_BASE_LEVEL 0
#endif
#ifndef GL_TEXTURE_WRAP_MAX_LEVEL
#define GL_TEXTURE_WRAP_MAX_LEVEL 0
#endif
#ifndef GL_TEXTURE_MIN_LOD
#define GL_TEXTURE_MIN_LOD 0
#endif
#ifndef GL_TEXTURE_MAX_LOD
#define GL_TEXTURE_MAX_LOD 0
#endif
"

(define-cproc gl-tex-parameter (target::<fixnum> pname::<fixnum> param)::<void>
  (case pname
    [(GL_TEXTURE_WRAP_S GL_TEXTURE_WRAP_T GL_TEXTURE_WRAP_R
      GL_TEXTURE_BASE_LEVEL GL_TEXTURE_MAX_LEVEL
      GL_TEXTURE_MAG_FILTER GL_TEXTURE_MIN_FILTER)
     (if (SCM_INTP param)
       (glTexParameteri target pname (Scm_GetInteger param))
       (Scm_Error "integer parameter required, but got %S" param))]
    [(GL_TEXTURE_PRIORITY GL_TEXTURE_MIN_LOD GL_TEXTURE_MAX_LOD)
     (if (SCM_REALP param)
       (glTexParameterf target pname (cast GLfloat (Scm_GetDouble param)))
       (Scm_Error "real parameter required, but got %S" param))]
    [(GL_TEXTURE_BORDER_COLOR)
     (gl-case (param) ("glTexParameter~v" target pname ~X) ((f32 4) (s32 4))
              "f32 or s32 vector parameter of size 4 required, but got %S")]
    [else
     (Scm_Error "unknown or unsupported glTexParameter pname: %d" pname)]))

(define-cproc gl-get-tex-parameter (target::<fixnum> pname::<fixnum>)
  (case pname
    [(GL_TEXTURE_WRAP_S GL_TEXTURE_WRAP_T GL_TEXTURE_WRAP_R
      GL_TEXTURE_BASE_LEVEL GL_TEXTURE_MAX_LEVEL
      GL_TEXTURE_MAG_FILTER GL_TEXTURE_MIN_FILTER)
     (let* ([i::GLint])
       (glGetTexParameteriv target pname (& i))
       (result (Scm_MakeInteger i)))]
    [(GL_TEXTURE_PRIORITY GL_TEXTURE_MIN_LOD GL_TEXTURE_MAX_LOD)
     (let* ([f::GLfloat])
       (glGetTexParameterfv target pname (& f))
       (result (Scm_MakeFlonum (cast double f))))]
    [(GL_TEXTURE_BORDER_COLOR)
     (let* ([vec (Scm_MakeF32Vector 4 0.0)])
       (glGetTexParameterfv target pname (SCM_F32VECTOR_ELEMENTS vec))
       (result vec))]
    [else
     (Scm_Error "unknown or unsupported glTexParameter pname: %d" pname)]))

(define-cproc gl-get-tex-level-parameter (target::<fixnum> level::<fixnum>
                                          pname::<fixnum>)
  (case pname
    [(GL_TEXTURE_WIDTH GL_TEXTURE_HEIGHT GL_TEXTURE_DEPTH
      GL_TEXTURE_BORDER GL_TEXTURE_INTERNAL_FORMAT
      GL_TEXTURE_RED_SIZE GL_TEXTURE_GREEN_SIZE GL_TEXTURE_BLUE_SIZE
      GL_TEXTURE_ALPHA_SIZE GL_TEXTURE_LUMINANCE_SIZE GL_TEXTURE_INTENSITY_SIZE)
     (let* ([i::GLint])
       (glGetTexLevelParameteriv target level pname (& i))
       (result (Scm_MakeInteger i)))]
    [else
     (Scm_Error "unknown or unsupported glTexLevelParameter pname: %d" pname)]))

;; Caller must ensure vector has enough length, since we need to get
;; pixel store parameters to check that, which is expensive.
;; We allow #f to TEXELS just to allcate texture area (to be used
;; as a render target via framebuffer object).
(define-cproc gl-tex-image-1d (target::<fixnum> level::<fixnum>
                               internalformat::<fixnum>
                               width::<fixnum> border::<fixnum>
                               format::<fixnum> type::<fixnum> texels)
  ::<void>
  (let* ([elttype::int]
         [size::int (Scm_GLPixelDataSize width 1 format type (& elttype) NULL)]
         [data::void* NULL])
    (unless (SCM_FALSEP texels)
      (set! data (Scm_GLPixelDataCheck texels elttype size)))
    (glTexImage1D target level internalformat width border format type data)))

;; Caller must ensure vector has enough length, since we need to get
;; pixel store parameters to check that, which is expensive.
;; We allow #f to TEXELS just to allcate texture area (to be used
;; as a render target via framebuffer object).
(define-cproc gl-tex-image-2d (target::<fixnum> level::<fixnum>
                               internalformat::<fixnum>
                               width::<fixnum> height::<fixnum>
                               border::<fixnum> format::<fixnum>
                               type::<fixnum> texels)
  ::<void>
  (let* ([elttype::int]
         [size::int (Scm_GLPixelDataSize width height format type
                                         (& elttype) NULL)]
         [data::void* NULL])
    (unless (SCM_FALSEP texels)
      (set! data (Scm_GLPixelDataCheck texels elttype size)))
    (glTexImage2D target level internalformat width height border format
                  type data)))

; gl-get-tex-image

(define-cproc gl-gen-textures (size::<fixnum>)
  (when (<= size 0)
    (Scm_Error "size must be a positive integer, but got %d" size))
  (let* ([vec (Scm_MakeU32Vector size 0)])
    (glGenTextures size (cast GLuint* (SCM_U32VECTOR_ELEMENTS vec)))
    (result vec)))

(define-cproc gl-delete-textures (names::<u32vector>) ::<void>
  (glDeleteTextures (SCM_U32VECTOR_SIZE names)
                    (cast GLuint* (SCM_U32VECTOR_ELEMENTS names))))

(define-cproc gl-bind-texture (target::<fixnum> name::<int>) ::<void>
  glBindTexture)

(define-cproc gl-prioritize-textures (names::<u32vector>
                                      priorities::<f32vector>)
  ::<void>
  (let* ([n::int (SCM_U32VECTOR_SIZE names)])
    (when (!= n (SCM_F32VECTOR_SIZE priorities))
      (Scm_Error "priority vector length doesn't match \
                  the names vector length %d: %S" n priorities))
    (glPrioritizeTextures n (cast GLuint* (SCM_U32VECTOR_ELEMENTS names))
                          (SCM_F32VECTOR_ELEMENTS priorities))))

(define-cproc gl-are-textures-resident! (names::<u32vector>
                                         res::<gl-boolean-vector>)
  ::<boolean>
  (glAreTexturesResident (SCM_U32VECTOR_SIZE names)
                         (cast GLuint* (SCM_U32VECTOR_ELEMENTS names))
                         (SCM_GL_BOOLEAN_VECTOR_ELEMENTS res)))

(define-cproc gl-is-texture (name::<int>) ::<int> glIsTexture)

(define-cproc gl-tex-sub-image-1d (target::<fixnum> level::<fixnum>
                                   xoffset::<fixnum> width::<fixnum>
                                   format::<fixnum> type::<fixnum>
                                   texels)
  ::<void>
  (let* ([elttype::int]
         [size::int (Scm_GLPixelDataSize width 1 format type (& elttype) NULL)]
         [texelptr::void* (Scm_GLPixelDataCheck texels elttype size)])
    (when texelptr
      (glTexSubImage1D target level xoffset width format type texelptr))))

(define-cproc gl-tex-sub-image-2d (target::<fixnum> level::<fixnum>
                                   xoffset::<fixnum> yoffset::<fixnum>
                                   width::<fixnum> height::<fixnum>
                                   format::<fixnum> type::<fixnum>
                                   texels)
  ::<void>
  (let* ([elttype::int]
         [size::int
          (Scm_GLPixelDataSize width height format type (& elttype) NULL)]
         [texelptr::void* (Scm_GLPixelDataCheck texels elttype size)])
    (when texelptr
      (glTexSubImage2D target level xoffset yoffset width height
                       format type texelptr))))

(define-cproc gl-copy-tex-image-1d (target::<fixnum> level::<fixnum>
                                    internal-format::<fixnum>
                                    x::<fixnum> y::<fixnum>
                                    width::<fixnum> border::<fixnum>)
  ::<void> glCopyTexImage1D)

(define-cproc gl-copy-tex-image-2d (target::<fixnum> level::<fixnum>
                                    internal-format::<fixnum>
                                    x::<fixnum> y::<fixnum>
                                    width::<fixnum> height::<fixnum>
                                    border::<fixnum>)
  ::<void> glCopyTexImage2D)

(define-cproc gl-copy-tex-sub-image-1d (target::<fixnum> level::<fixnum>
                                        xoffset::<fixnum> x::<fixnum>
                                        y::<fixnum> width::<fixnum>)
  ::<void> glCopyTexSubImage1D)

(define-cproc gl-copy-tex-sub-image-2d (target::<fixnum> level::<fixnum>
                                        xoffset::<fixnum> yoffset::<fixnum>
                                        x::<fixnum> y::<fixnum>
                                        width::<fixnum> height::<fixnum>)
  ::<void> glCopyTexSubImage2D)

;;=============================================================
;; Evaluators
;;

; gl-map1
; gl-map2
; gl-get-map
; gl-eval-coord1
; gl-eval-coord2
; gl-map-grid
; gl-eval-point1
; gl-eval-point2
; gl-eval-mesh1
; gl-eval-mesh2

;;=============================================================
;; Fog
;;

(define-cproc gl-fog (pname::<fixnum> param) ::<void>
  (case pname
    [(GL_FOG_MODE GL_FOG_INDEX)
     (if (SCM_INTP param)
       (glFogi pname (SCM_INT_VALUE param))
       (Scm_Error "integer parameter required, but got %S" param))]
    [(GL_FOG_DENSITY GL_FOG_START GL_FOG_END)
     (if (SCM_REALP param)
       (glFogf pname (cast GLfloat (Scm_GetDouble param)))
       (Scm_Error "real number parameter required, but got %S" param))]
    [(GL_FOG_COLOR)
     (assert-vector-type&size f32 4 param)
     (glFogfv pname (SCM_F32VECTOR_ELEMENTS param))]
    [else (Scm_Error "unknown or unsupported glFog pname: %d" pname)]))

;;=============================================================
;; Selection and feedback
;;

(define-cproc gl-feedback-buffer (type::<fixnum>
                                  buffer::<f32vector>)
  ::<void>
  (glFeedbackBuffer (SCM_F32VECTOR_SIZE buffer) type
                    (SCM_F32VECTOR_ELEMENTS buffer)))

(define-cproc gl-select-buffer (buffer::<u32vector>) ::<void>
  (glSelectBuffer (SCM_U32VECTOR_SIZE buffer)
                  (cast GLuint* (SCM_U32VECTOR_ELEMENTS buffer))))

(define-cproc gl-pass-through (token::<float>) ::<void> glPassThrough)

(define-cproc gl-init-names () ::<void> glInitNames)
(define-cproc gl-load-name (name::<int>) ::<void> glLoadName)
(define-cproc gl-push-name (name::<int>) ::<void> glPushName)
(define-cproc gl-pop-name () ::<void> glPopName)

) ;; end inline-stub

;; Local variables:
;; mode: scheme
;; end:
