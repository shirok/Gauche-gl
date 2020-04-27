;;;
;;; glext-lib.scm - glue functions for GL extensions
;;;
;;;  Copyright (c) 2004-2015  Shiro Kawai  <shiro@acm.org>
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
          "#include \"gl-syms.h\""
          "#include \"gl-ptrs.h\"")

(include "glcase.scm")

"#define CHECK_ERROR(msg__)                            \\
  do {                                                 \\
    GLenum e = glGetError();                           \\
    if (e != GL_NO_ERROR) {                            \\
      Scm_Error(\"%s: %s\", msg__, gluErrorString(e)); \\
    }                                                  \\
  } while (0)

/* GLhandle is either typedef'ed to uint or void*;
   for the time being, we treat cast it to intptr_t
 */
#define SCM_GL_HANDLE_P(obj)  SCM_INTEGERP(obj)
#define SCM_MAKE_GL_HANDLE(handle)  Scm_MakeIntegerU((intptr_t)(handle))
#define SCM_GL_HANDLE_VALUE(shandle) (GLhandleARB)(intptr_t)Scm_GetIntegerU(shandle)


"

;; NB: this should be taken care of by genstub.
(define-type <uvector> "ScmUVector*" "uniform vector"
  "SCM_UVECTORP" "SCM_UVECTOR")
(define-type <u32vector> "ScmU32Vector*" "u32vector"
  "SCM_U32VECTORP" "SCM_U32VECTOR")
(define-type <s32vector> "ScmS32Vector*" "s32vector"
  "SCM_S32VECTORP" "SCM_S32VECTOR")
(define-type <f32vector> "ScmF32Vector*" "f32vector"
  "SCM_F32VECTORP" "SCM_F32VECTOR")

;;=============================================================
;; GL 1.2
;;

(define-cproc gl-tex-image-3d (target::<fixnum> level::<fixnum>
                               internal-format::<fixnum>
                               width::<fixnum> height::<fixnum>
                               depth::<fixnum> border::<fixnum>
                               format::<fixnum> type::<fixnum>
                               texels)
  ::<void>
  (let* ([elttype::int]
         [size::int
          (Scm_GLPixelDataSize width height format type (& elttype) NULL)]
         [texelptr::void*
          (Scm_GLPixelDataCheck texels elttype (* size depth))])
    (when texelptr
      (ENSURE glTexImage3D)
      (glTexImage3D target level internal_format width height depth
                    border format type texelptr))))

(define-cproc gl-tex-sub-image-3d (target::<fixnum> level::<fixnum>
                                   xoffset::<fixnum> yoffset::<fixnum>
                                   zoffset::<fixnum>
                                   width::<fixnum> height::<fixnum>
                                   depth::<fixnum>
                                   format::<fixnum> type::<fixnum>
                                   texels)
  ::<void>
  (let* ([elttype::int]
         [size::int
          (Scm_GLPixelDataSize width height format type (& elttype) NULL)]
         [texelptr::void*
          (Scm_GLPixelDataCheck texels elttype (* size depth))])
    (when texelptr
      (ENSURE glTexSubImage3D)
      (glTexSubImage3D target level xoffset yoffset zoffset
                       width height depth format type texelptr))))

(define-cproc gl-copy-tex-sub-image-3d (target::<fixnum> level::<fixnum>
                                        xoffset::<fixnum> yoffset::<fixnum>
                                        zoffset::<fixnum>
                                        x::<fixnum> y::<fixnum>
                                        width::<fixnum> height::<fixnum>)
  ::<void>
  (ENSURE glCopyTexSubImage3D)
  (glCopyTexSubImage3D target level xoffset yoffset zoffset x y width height))

(define-cproc gl-draw-range-elements (mode::<fixnum>
                                      start::<uint> end::<uint>
                                      indices)
  ::<void>
  (ENSURE glDrawRangeElements)
  (gl-case (indices)
           (glDrawRangeElements mode start end (SCM_UVECTOR_SIZE indices) ~E ~X)
           ((u8) (u16) (u32))
           "bad argument for indices; must be u8, u16 or u32vector, but got %S"))
;;=============================================================
;; GL_ARB_imaging
;;

(define-cproc gl-color-table (target::<fixnum> internal-format::<fixnum>
                              width::<int> format::<int> type::<int>
                              data)
  ::<void>
  (let* ([elttype::int] [packed::int]
         [size::int
          (Scm_GLPixelDataSize width 1 format type (& elttype) (& packed))])
    (ENSURE glColorTable)
    (glColorTable target internal_format width format type
                  (Scm_GLPixelDataCheck data elttype size))))

(define-cproc gl-color-sub-table (target::<fixnum> start::<int> count::<int>
                                  format::<int> type::<int>
                                  data)
  ::<void>
  (let* ([elttype::int] [packed::int]
         [size::int
          (Scm_GLPixelDataSize count 1 format type (& elttype) (& packed))])
    (ENSURE glColorSubTable)
    (glColorSubTable target start count format type
                     (Scm_GLPixelDataCheck data elttype size))))

(define-cproc gl-color-table-parameter (target::<fixnum> pname::<fixnum>
                                        param)
  ::<void>
  (gl-case (param) (begin (ENSURE "glColorTableParameter~v")
                          ("glColorTableParameter~v" target pname ~X))
           ((f32 4) (s32 4))
           "f32 or s32 vector of size 4 required, but got %S"))

(define-cproc gl-copy-color-sub-table (target::<fixnum> start::<fixnum>
                                       x::<int> y::<int> width::<int>)
  ::<void>
  (ENSURE glCopyColorSubTable)
  (glCopyColorSubTable target start x y width))


(define-cproc gl-copy-color-table (target::<fixnum> internal-format::<fixnum>
                                   x::<int> y::<int> width::<int>)
  ::<void>
  (ENSURE glCopyColorTable)
  (glCopyColorTable target internal_format x y width))

(define-cproc gl-get-color-table! (target::<fixnum> format::<fixnum>
                                   type::<fixnum> data::<uvector>)
  ;; TODO: check the type and size vailidity of data
  (ENSURE glGetColorTable)
  (glGetColorTable target format type (SCM_UVECTOR_ELEMENTS data))
  (result (SCM_OBJ data)))

; gl-get-color-table-parameter

(define-cproc gl-blend-equation (mode::<fixnum>)
  ::<void>
  (ENSURE glBlendEquation)
  (glBlendEquation mode))

(define-cproc gl-blend-color (red::<float> green::<float> blue::<float> alpha::<float>)
  ::<void>
  (ENSURE glBlendColor)
  (glBlendColor red green blue alpha))

(define-cproc gl-histogram (target::<fixnum> width::<int> internal-format::<fixnum> sink::<boolean>)
  ::<void>
  (ENSURE glHistogram)
  (glHistogram target width internal_format sink))

(define-cproc gl-reset-histogram (target::<fixnum>)
  ::<void>
  (ENSURE glResetHistogram)
  (glResetHistogram target))

(define-cproc gl-get-histogram (target::<fixnum> reset::<boolean>
                                format::<fixnum> type::<fixnum>)
  (ENSURE glGetHistogramParameteriv)
  (ENSURE glGetHistogram)
  (let* ([elttype::int] [width::GLint])
    (glGetHistogramParameteriv GL_HISTOGRAM GL_HISTOGRAM_WIDTH (& width))
    (let* ([size::int
            (Scm_GLPixelDataSize width 1 format type (& elttype) NULL)]
           [vec (Scm_GLAllocUVector elttype size)])
      (unless (SCM_UVECTORP vec)
        (Scm_Error "invalid format or type (%S, %S)" format type))
      (glGetHistogram target reset format type (SCM_UVECTOR_ELEMENTS vec))
      (result vec))))

(define-cproc gl-get-histogram-parameter (target::<fixnum>
                                          pname::<fixnum>)
  (ENSURE glGetHistogramParameteriv)
  (let* ([param::GLint])
    (case pname
      [(GL_HISTOGRAM_SINK)
       (glGetHistogramParameteriv target pname (& param))
       (result (SCM_MAKE_BOOL param))]
      [else
       (glGetHistogramParameteriv target pname (& param))
       (result (Scm_MakeInteger param))])))

(define-cproc gl-minmax (target::<fixnum> internal-format::<fixnum> sink::<boolean>) ::<void>
  (ENSURE glMinmax)
  (glMinmax target internal_format sink))

(define-cproc gl-reset-minmax (target::<fixnum>) ::<void>
  (ENSURE glResetMinmax)
  (glResetMinmax target))

(define-cproc gl-get-minmax (target::<fixnum> reset::<boolean>
                             format::<fixnum> type::<fixnum>)
  (let* ([elttype::GLint]
         [size::int
          (Scm_GLPixelDataSize 2 1 format type (& elttype) NULL)]
         [vec (Scm_GLAllocUVector elttype size)])
    (unless (SCM_UVECTORP vec)
      (Scm_Error "invalid format or type (%S, %S)" format type))
    (ENSURE glGetMinmax)
    (glGetMinmax target reset format type (SCM_UVECTOR_ELEMENTS vec))
    (result vec)))
   
(define-cproc gl-get-minmax-parameter (target::<fixnum>
                                       pname::<fixnum>)
  (ENSURE glGetMinmaxParameteriv)
  (let* ([param::GLint])
    (case pname
      [(GL_MINMAX_SINK)
       (glGetMinmaxParameteriv target pname (& param))
       (result (SCM_MAKE_BOOL param))]
      [else
       (glGetMinmaxParameteriv target pname (& param))
       (result (Scm_MakeInteger param))])))

(define-cproc gl-convolution-filter-2d (target::<fixnum>
                                        internal-format::<fixnum>
                                        width::<fixnum> height::<fixnum>
                                        format::<fixnum> type::<fixnum>
                                        data::<uvector>)
  ::<void>
  (let* ([elttype::GLint]
         [size::int
          (Scm_GLPixelDataSize width height format type (& elttype) NULL)])
    (when (< (SCM_UVECTOR_SIZE data) size)
      (Scm_Error "given vector is too short (minimum %d elements): %S"
                 size data))
    (ENSURE glConvolutionFilter2D)
    (glConvolutionFilter2D target internal_format width height 
                           format type (SCM_UVECTOR_ELEMENTS data))))

(define-cproc gl-copy-convolution-filter-2d (target::<fixnum>
                                             internal-format::<fixnum>
                                             x::<fixnum> y::<fixnum>
                                             width::<fixnum> height::<fixnum>)
  ::<void>
  (ENSURE glCopyConvolutionFilter2D)
  (glCopyConvolutionFilter2D target internal_format x y width height))

(define-cproc gl-separable-filter-2d (target::<fixnum>
                                      internal-format::<fixnum>
                                      width::<fixnum> height::<fixnum>
                                      format::<fixnum> type::<fixnum>
                                      row::<uvector> column::<uvector>)
  ::<void>
  (ENSURE glSeparableFilter2D)
  (glSeparableFilter2D target internal_format width height format type
                       (SCM_UVECTOR_ELEMENTS row)
                       (SCM_UVECTOR_ELEMENTS column)))

(define-cproc gl-convolution-filter-1d (target::<fixnum>
                                        internal-format::<fixnum>
                                        width::<fixnum>
                                        format::<fixnum> type::<fixnum>
                                        data::<uvector>)
  ::<void>
  (let* ([elttype::GLint]
         [size::int
          (Scm_GLPixelDataSize width 1 format type (& elttype) NULL)])
    (when (< (SCM_UVECTOR_SIZE data) size)
      (Scm_Error "given vector is too short (minimum %d elements): %S"
                 size data))
    (ENSURE glConvolutionFilter1D)
    (glConvolutionFilter1D target internal_format width 
                           format type (SCM_UVECTOR_ELEMENTS data))))

(define-cproc gl-copy-convolution-filter-1d (target::<fixnum>
                                             internal-format::<fixnum>
                                             x::<fixnum> y::<fixnum>
                                             width::<fixnum>)
  ::<void>
  (ENSURE glCopyConvolutionFilter1D)
  (glCopyConvolutionFilter1D target internal_format x y width))

(define-cfn gl-convolution-parameter-1 (target::int
                                        pname::int
                                        pname-string::(const char*)
                                        param)
  ::void
  (unless (SCM_INTP param)
    (Scm_Error "bad type of param value for %s (int required): %S"
               pname-string param))
  (ENSURE glConvolutionParameteri)
  (glConvolutionParameteri target pname (SCM_INT_VALUE param)))

(define-cfn gl-convolution-parameter-4 (target::int
                                        pname::int
                                        pname-string::(const char*)
                                        param)
  ::void
  (gl-case (param)
           (begin
             (ENSURE "glConvolutionParameter~v")
             ("glConvolutionParameter~v" target pname ~X))
           ((f32 4) (s32 4))
           (Scm_Error "bad type of param value for %s (s32 or f32vector of size 4 required), but got: %S" pname-string param)))

(define-cise-expr gl-convolution-parameter-dispatch
  [(_ (enum-name type) ...)
   (define (gen-call enum-name type)
     (ecase type
       [(s) ; scalar
        `((,enum-name)
          (gl-convolution-parameter-1
           target ,enum-name ,(x->string enum-name) param))]
       [(v) ; vector
        `((,enum-name)
          (gl-convolution-parameter-4
           target ,enum-name ,(x->string enum-name) param))]))
   `(case pname
      ,@(map gen-call enum-name type)
      [else (Scm_Error "Invalid pname parameter for gl-convolution-parameter: %d" pname)])])

(define-cproc gl-convolution-parameter (target::<fixnum>
                                        pname::<fixnum>
                                        param)
  ::<void>
  (gl-convolution-parameter-dispatch
   (GL_CONVOLUTION_BORDER_MODE s)
   (GL_CONVOLUTION_BORDER_COLOR v)
   (GL_CONVOLUTION_FILTER_SCALE v)
   (GL_CONVOLUTION_FILTER_BIAS v)
   ))

; gl-get-convolution-filter

(define-cproc gl-get-convolution-parameter (target::<fixnum>
                                            pname::<fixnum>)
  (case pname
    [(GL_CONVOLUTION_BORDER_MODE
      GL_CONVOLUTION_FORMAT
      GL_CONVOLUTION_WIDTH
      GL_CONVOLUTION_HEIGHT
      GL_MAX_CONVOLUTION_WIDTH
      GL_MAX_CONVOLUTION_HEIGHT)
     (ENSURE glGetConvolutionParameteriv)
     (let* ([r::GLint])
       (glGetConvolutionParameteriv target pname (& r))
       (result (Scm_MakeInteger r)))]
    [(GL_CONVOLUTION_BORDER_COLOR
      GL_CONVOLUTION_FILTER_SCALE
      GL_CONVOLUTION_FILTER_BIAS)
     (ENSURE glGetConvolutionParameterfv)
     (let* ([v (Scm_MakeF32Vector 4 0)])
       (glGetConvolutionParameterfv target pname (SCM_F32VECTOR_ELEMENTS v))
       (result v))]
    [else
     (Scm_Error "Invalid pname parameter for gl-get-convolution-parameter: %d" pname)]))

; gl-get-separable-filter

;;=============================================================
;; GL_ARB_depth_texture
;;

;;=============================================================
;; GL_ARB_fragment_program
;;

;;=============================================================
;; GL_ARB_fragment_program_shadow
;;

;;=============================================================
;; GL_ARB_fragment_shader
;;

;;=============================================================
;; GL_ARB_matrix_palette
;;

;;=============================================================
;; GL_ARB_multisample
;;

(define-cproc gl-sample-coverage-arb (value::<float> invert::<boolean>)
  ::<void>
  (ENSURE glSampleCoverageARB)
  (glSampleCoverageARB value invert))

;;=============================================================
;; GL_ARB_multitexture
;;

(define-cproc gl-active-texture-arb (texture::<int>) ::<void>
  (ENSURE glActiveTextureARB)
  (glActiveTextureARB texture))

(define-cproc gl-active-texture (texture::<int>) ::<void> ; GL 1.3
  (ENSURE glActiveTexture)
  (glActiveTexture texture))

(define-cproc gl-client-active-texture-arb (texture::<int>) ::<void>
  (ENSURE glClientActiveTextureARB)
  (glClientActiveTextureARB texture))

(define-cproc gl-client-active-texture (texture::<int>) ::<void>; GL 1.3
  (ENSURE glClientActiveTexture)
  (glClientActiveTexture texture))

(define-cproc gl-multi-tex-coord-arb (texunit::<int> v &rest args) ::<void>
  (gl-case (v args)
           (begin
             (ENSURE "glMultiTexCoord~n~vARB")
             ("glMultiTexCoord~n~vARB" texunit ~X))
           ((f32 2 1 3 4) (f64 2 1 3 4) (s32 2 1 3 4) (s16 2 1 3 4)
            (args 2 1 3 4))
           "bad argument for v: %S, must be one of f32, f64, s32 or s16 vector of length 1, 2, 3, or 4."))

;;=============================================================
;; GL_ARB_occlusion_query
;;

;; gl-genqueries-arb
;; gl-delete-queries-arb

(define-cproc gl-is-query-arb (query::<uint>) ::<boolean>
  (ENSURE glIsQueryARB)
  (result (glIsQueryARB query)))

(define-cproc gl-begin-query-arb (op::<uint> query::<uint>) ::<void>
  (ENSURE glBeginQueryARB)
  (glBeginQueryARB op query))

(define-cproc gl-end-query-arb (op::<uint>) ::<void>
  (ENSURE glEndQueryARB)
  (glEndQueryARB op))

;; gl-get-query-arb
;; gl-get-query-object-arb

;;=============================================================
;; GL_ARB_point_parameters
;;

;;=============================================================
;; GL_ARB_point_sprite
;;

;;=============================================================
;; GL_ARB_shader_objects
;;

(define-type <gl-handle> "GLhandleARB" "glhandle"
  "SCM_GL_HANDLE_P" "SCM_GL_HANDLE_VALUE" "SCM_MAKE_GL_HANDLE")

(define-cproc gl-delete-object-arb (h::<gl-handle>) ::<void>
  (ENSURE glDeleteObjectARB)
  (glDeleteObjectARB h))

(define-cproc gl-get-handle-arb (type::<uint>) ::<gl-handle>
  (ENSURE glGetHandleARB)
  (result (glGetHandleARB type)))

(define-cproc gl-create-shader-object-arb (type::<uint>) ::<gl-handle>
  (ENSURE glCreateShaderObjectARB)
  (result (glCreateShaderObjectARB type)))

(define-cproc gl-shader-source-arb (shader::<gl-handle> strings) ::<void>
  (let* ([nstrings::GLint (Scm_Length strings)]
         [i::int 0])
    (ENSURE glShaderSourceARB)
    (when (< nstrings 0)
      (label einval)
      (Scm_Error "list of strings required, but got %S" strings))
    (let* ([lengths::GLint*
            (SCM_NEW_ATOMIC2 (GLint*) (* nstrings (sizeof GLint)))]
           ;; NB: we can use atomic here, since all strings are pointed by the
           ;; input parameter, and we don't need this array after calling
           ;; glShaderSourceARB.
           [ss::GLcharARB**
            (SCM_NEW_ATOMIC2 (GLcharARB**) (* nstrings (sizeof (GLcharARB*))))])
      (dolist [s strings]
        (unless (SCM_STRINGP s) (goto einval))
        (set! (aref lengths i) (SCM_STRING_SIZE s))
        (set! (aref ss i) (cast GLcharARB* (SCM_STRING_START s)))
        (inc! i))
      (glShaderSourceARB shader nstrings (cast (const GLcharARB**) ss)
                         lengths))))

(define-cproc gl-compile-shader-arb (shader::<gl-handle>) ::<void>
  (ENSURE glCompileShaderARB)
  (glCompileShaderARB shader))

(define-cproc gl-create-program-object-arb () ::<gl-handle>
  (ENSURE glCreateProgramObjectARB)
  (result (glCreateProgramObjectARB)))

(define-cproc gl-attach-object-arb (program::<gl-handle> shader::<gl-handle>)
  ::<void>
  (ENSURE glAttachObjectARB)
  (glAttachObjectARB program shader))

(define-cproc gl-detach-object-arb (program::<gl-handle> shader::<gl-handle>)
  ::<void>
  (ENSURE glDetachObjectARB)
  (glDetachObjectARB program shader))

(define-cproc gl-link-program-arb (program::<gl-handle>) ::<void>
  (ENSURE glLinkProgramARB)
  (glLinkProgramARB program))

(define-cproc gl-use-program-object-arb (program::<gl-handle>) ::<void>
  (ENSURE glUseProgramObjectARB)
  (glUseProgramObjectARB program))

(define-cproc gl-validate-program-arb (program::<gl-handle>) ::<void>
  (ENSURE glValidateProgramARB)
  (glValidateProgramARB program))

(define-cproc gl-uniform1-arb (location::<int> v0) ::<void>
  (cond
   [(SCM_F32VECTORP v0)
    (let* ([count::int (SCM_F32VECTOR_SIZE v0)])
      (ENSURE glUniform1fvARB)
      (glUniform1fvARB location count (SCM_F32VECTOR_ELEMENTS v0)))]
   [(SCM_S32VECTORP v0)
    (let* ([count::int (SCM_S32VECTOR_SIZE v0)])
      (ENSURE glUniform1ivARB)
      (glUniform1ivARB location count (SCM_S32VECTOR_ELEMENTS v0)))]
   [else
    (ENSURE glUniform1fARB)
    (glUniform1fARB location (cast GLfloat (Scm_GetDouble v0)))]))

(define-cproc gl-uniform2-arb (location::<int> v0 &optional v1) ::<void>
  (cond
   [(SCM_F32VECTORP v0)
    (let* ([count::int (/ (SCM_F32VECTOR_SIZE v0) 2)])
      (ENSURE glUniform2fvARB)
      (glUniform2fvARB location count (SCM_F32VECTOR_ELEMENTS v0)))]
   [(SCM_S32VECTORP v0)
    (let* ([count::int (/ (SCM_S32VECTOR_SIZE v0) 2)])
      (ENSURE glUniform2ivARB)
      (glUniform2ivARB location count (SCM_S32VECTOR_ELEMENTS v0)))]
   [(SCM_UNBOUNDP v1)
    (Scm_Error "Not enough arguments for gl-uniform2-arb")]
   [else
    (ENSURE glUniform2fARB)
    (glUniform2fARB location
                    (cast GLfloat (Scm_GetDouble v0))
                    (cast GLfloat (Scm_GetDouble v1)))]))

(define-cproc gl-uniform3-arb (location::<int> v0 &optional v1 v2) ::<void>
  (cond
   [(SCM_F32VECTORP v0)
    (let* ([count::int (/ (SCM_F32VECTOR_SIZE v0) 3)])
      (ENSURE glUniform3fvARB)
      (glUniform3fvARB location count (SCM_F32VECTOR_ELEMENTS v0)))]
   [(SCM_S32VECTORP v0)
    (let* ([count::int (/ (SCM_S32VECTOR_SIZE v0) 3)])
      (ENSURE glUniform3ivARB)
      (glUniform3ivARB location count (SCM_S32VECTOR_ELEMENTS v0)))]
   [(SCM_UNBOUNDP v2)
    (Scm_Error "Not enough arguments for gl-uniform3-arb")]
   [else
    (ENSURE glUniform3fARB)
    (glUniform3fARB location (cast GLfloat (Scm_GetDouble v0))
                    (cast GLfloat (Scm_GetDouble v1))
                    (cast GLfloat (Scm_GetDouble v2)))]))

(define-cproc gl-uniform4-arb (location::<int> v0 &optional v1 v2 v3) ::<void>
  (cond
   [(SCM_F32VECTORP v0)
    (let* ([count::int (/ (SCM_F32VECTOR_SIZE v0) 4)])
      (ENSURE glUniform4fvARB)
      (glUniform4fvARB location count (SCM_F32VECTOR_ELEMENTS v0)))]
   [(SCM_S32VECTORP v0)
    (let* ([count::int (/ (SCM_S32VECTOR_SIZE v0) 4)])
      (ENSURE glUniform4ivARB)
      (glUniform4ivARB location count (SCM_S32VECTOR_ELEMENTS v0)))]
   [(SCM_UNBOUNDP v3)
    (Scm_Error "Not enough arguments for gl-uniform4-arb")]
   [else
    (ENSURE glUniform4fARB)
    (glUniform4fARB location (cast GLfloat (Scm_GetDouble v0))
                    (cast GLfloat (Scm_GetDouble v1))
                    (cast GLfloat (Scm_GetDouble v2))
                    (cast GLfloat (Scm_GetDouble v3)))]))

(define-cproc gl-uniform-matrix2-arb (location::<int>
                                      transpose::<boolean>
                                      v::<f32vector>)
  ::<void>
  (let* ([count::int (/ (SCM_F32VECTOR_SIZE v) 4)])
    (ENSURE glUniformMatrix2fvARB)
    (glUniformMatrix2fvARB location count transpose 
                           (SCM_F32VECTOR_ELEMENTS v))))

(define-cproc gl-uniform-matrix3-arb (location::<int>
                                      transpose::<boolean>
                                      v::<f32vector>)
  ::<void>
  (let* ([count::int (/ (SCM_F32VECTOR_SIZE v) 9)])
    (ENSURE glUniformMatrix3fvARB)
    (glUniformMatrix3fvARB location count transpose 
                           (SCM_F32VECTOR_ELEMENTS v))))

(define-cproc gl-uniform-matrix4-arb (location::<int>
                                      transpose::<boolean>
                                      v::<f32vector>)
  ::<void>
  (let* ([count::int (/ (SCM_F32VECTOR_SIZE v) 16)])
    (ENSURE glUniformMatrix4fvARB)
    (glUniformMatrix4fvARB location count transpose 
                           (SCM_F32VECTOR_ELEMENTS v))))

(define-cproc gl-get-object-parameter-arb (object::<gl-handle>
                                           pname::<uint>)
  (ENSURE glGetObjectParameterfvARB)
  (ENSURE glGetObjectParameterivARB)
  (case pname
    [(GL_OBJECT_TYPE_ARB
      GL_OBJECT_SUBTYPE_ARB
      GL_OBJECT_DELETE_STATUS_ARB
      GL_OBJECT_COMPILE_STATUS_ARB
      GL_OBJECT_LINK_STATUS_ARB
      GL_OBJECT_VALIDATE_STATUS_ARB
      GL_OBJECT_INFO_LOG_LENGTH_ARB
      GL_OBJECT_ATTACHED_OBJECTS_ARB
      GL_OBJECT_ACTIVE_ATTRIBUTES_ARB
      GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB
      GL_OBJECT_ACTIVE_UNIFORMS_ARB
      GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB
      GL_OBJECT_SHADER_SOURCE_LENGTH_ARB)
     (let* ([i::GLint])
       (glGetObjectParameterivARB object pname (& i))
       (result (Scm_MakeInteger i)))]
    [else
     (Scm_Error "invalid pname for gl-get-object-parameter-arb: %d" pname)]))

(define-cproc gl-get-info-log-arb (object::<gl-handle>)
  (let* ([loglen::GLint 0])
    (ENSURE glGetObjectParameterivARB)
    (ENSURE glGetInfoLogARB)
    (glGetObjectParameterivARB object GL_OBJECT_INFO_LOG_LENGTH_ARB
                               (& loglen))
    (when (< loglen 0) (= loglen 0))
    (let* ([logstr::GLcharARB*
            (SCM_NEW_ATOMIC2 (GLcharARB*) (* (+ loglen 1) (sizeof GLcharARB)))])
      (glGetInfoLogARB object loglen NULL logstr)
      (CHECK_ERROR glGetInfoLogARB)
      (= (aref logstr loglen) 0)
      (result (Scm_MakeString (cast (const char*) logstr) (- loglen 1) -1 0)))))

(define-cproc gl-get-attached-objects-arb (program::<gl-handle>)
  (ENSURE glGetObjectParameterivARB)
  (ENSURE glGetAttachedObjectsARB)
  (let* ([numobjs::GLint])
    (glGetObjectParameterivARB program GL_OBJECT_ATTACHED_OBJECTS_ARB
                               (& numobjs))
    (CHECK_ERROR "glGetObjectParameterivARB")
    (let* ([objs::GLhandleARB*
            (SCM_NEW_ATOMIC2 (GLhandleARB*) (* numobjs (sizeof GLhandleARB)))])
      (glGetAttachedObjectsARB program numobjs NULL objs)
      (CHECK_ERROR "glGetInfoLogARB")
      (let* ([r (Scm_MakeVector numobjs SCM_FALSE)])
        (dotimes [i numobjs]
          (set! (SCM_VECTOR_ELEMENT r i) (SCM_MAKE_GL_HANDLE (aref objs i))))
        (result r)))))

(define-cproc gl-get-uniform-location-arb (program::<gl-handle>
                                           expr::<string>)
  (ENSURE glGetUniformLocationARB)
  (let* ([r::GLint
          (glGetUniformLocationARB program
                                   (cast (const GLcharARB*)
                                         (Scm_GetStringConst expr)))])
    (CHECK_ERROR "glGetUniformLocationARB")
    (result (Scm_MakeInteger r))))

;; returns (size, type, name)
(define-cproc gl-get-active-uniform-arb (program::<gl-handle>
                                         index::<uint>)
  ::(<int> <int> <top>)
  (ENSURE glGetObjectParameterivARB)
  (ENSURE glGetActiveUniformARB)
  (let* ([maxlen::GLint])
    (glGetObjectParameterivARB program GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB
                               (& maxlen))
    (CHECK_ERROR "glGetObjectParameterivARB")
    (when (< maxlen 0) (= maxlen 0))
    (let* ([len::GLsizei] [size::GLint] [type::GLenum]
           [namebuf::GLcharARB* (SCM_NEW_ATOMIC2 (GLcharARB*)
                                                 (* (+ maxlen 1)
                                                    (sizeof GLcharARB)))])
      (glGetActiveUniformARB program index maxlen
                             (& len) (& size) (& type) namebuf)
      (CHECK_ERROR "glGetActiveUniformARB")
      (= (aref namebuf maxlen) 0)
      (result size type (Scm_MakeString (cast (const char*) namebuf) len -1 0)))))

; get-uniform-f-arb
; get-uniform-i-arb

(define-cproc gl-get-shader-source-arb (object::<gl-handle>)
  (ENSURE glGetObjectParameterivARB)
  (ENSURE glGetShaderSourceARB)
  (let* ([srclen::GLint])
    (glGetObjectParameterivARB object GL_OBJECT_SHADER_SOURCE_LENGTH_ARB
                               (& srclen))
    (CHECK_ERROR "glGetObjectParameterivARB")
    (when (< srclen 0) (= srclen 0))
    (let* ([srcstr::GLcharARB*
            (SCM_NEW_ATOMIC2 (GLcharARB*) (* (+ srclen 1) (sizeof GLcharARB)))])
      (glGetShaderSourceARB object srclen NULL srcstr)
      (= (aref srcstr srclen) 0)
      (result (Scm_MakeString (cast (const char*) srcstr) (- srclen 1) -1 0)))))

;;=============================================================
;; GL_ARB_shading_language_100
;;

;;=============================================================
;; GL_ARB_shadow
;;

;;=============================================================
;; GL_ARB_shadow_ambient
;;

;;=============================================================
;; GL_ARB_texture_border_clamp
;;

;;=============================================================
;; GL_ARB_texture_compression
;;

; gl-compressed-tex-image-3d-arb
; gl-compressed-tex-image-2d-arb
; gl-compressed-tex-image-1d-arb
; gl-compressed-tex-subimage-3d-arb
; gl-compressed-tex-subimage-2d-arb
; gl-compressed-tex-subimage-1d-arb
; gl-get-compressed-tex-image-arb

;;=============================================================
;; GL_ARB_texture_cube_map
;;

;;=============================================================
;; GL_ARB_texture_env_add
;;

;;=============================================================
;; GL_ARB_texture_env_combine
;;

;;=============================================================
;; GL_ARB_texture_env_dot3
;;

;;=============================================================
;; GL_ARB_texture_mirrored_repeat
;;

;;=============================================================
;; GL_ARB_texture_non_power_of_two
;;

;;=============================================================
;; GL_ARB_transpose_matrix
;;

(define-cproc gl-load-transpose-matrix-arb (m) ::<void>
  (gl-case (m) (begin (ENSURE "glLoadTransposeMatrix~tARB")
                      ("glLoadTransposeMatrix~tARB" ~X))
           ((m4f) (f32 16) (f64 16))
           "matrix4f, f32vector or f64vector of length 16 is required, but got %S"))

(define-cproc gl-mult-transpose-matrix-arb (m) ::<void>
  (gl-case (m) (begin (ENSURE "glMultTransposeMatrix~tARB")
                      ("glMultTransposeMatrix~tARB" ~X))
           ((m4f) (f32 16) (f64 16))
           "matrix4f, or f32vector or f64vector of length 16 is required, but got %S"))

;;=============================================================
;; (GL_ARB_vertex_buffer_object)
;; GL_EXT_vertex_buffer_object

(define-cproc gl-bind-buffer (target::<int> buffer::<uint>) ::<void>
  (ENSURE glBindBuffer)
  (glBindBuffer target buffer))

(define-cproc gl-delete-buffers (buffers::<u32vector>) ::<void>
  (ENSURE glDeleteBuffers)
  (glDeleteBuffers (SCM_U32VECTOR_SIZE buffers)
                   (SCM_U32VECTOR_ELEMENTS buffers)))

(define-cproc gl-gen-buffers (n::<uint>)
  (let* ([v (Scm_MakeU32Vector n 0)])
    (ENSURE glGenBuffers)
    (glGenBuffers n (SCM_U32VECTOR_ELEMENTS v))
    (result v)))

(define-cproc gl-is-buffer (buffer::<uint>) ::<boolean>
  (ENSURE glIsBuffer)
  (result (glIsBuffer buffer)))

(define-cproc gl-buffer-data (target::<fixnum> 
                              size::<ulong>
                              data
                              usage::<fixnum>)
  ::<void>
  (let* ([p::GLvoid* NULL])
    (cond [(SCM_UVECTORP data) (set! p (SCM_UVECTOR_ELEMENTS data))]
          [(SCM_POINT4F_ARRAY_P data) (set! p (SCM_POINT4F_ARRAY_D data))]
          [(SCM_FALSEP data)]
          [else
           (Scm_Error "data must be either uvector, point4f-array or #f, \
                       but got %S" data)])
    (ENSURE glBufferData)
    (glBufferData target size p usage)))

(define-cproc gl-buffer-sub-data (target::<fixnum>
                                  offset::<fixnum>
                                  size::<fixnum>
                                  data) ::<void>
  (let* ([p::GLvoid* NULL])
    (cond [(SCM_UVECTORP data) (set! p (SCM_UVECTOR_ELEMENTS data))]
          [(SCM_POINT4F_ARRAY_P data) (set! p (SCM_POINT4F_ARRAY_D data))]
          [else
           (Scm_Error "data must be either uvector, or point4f-array, \
                       but got %S" data)])
    (ENSURE glBufferSubData)
    (glBufferSubData target offset size p)))

(define-cproc gl-map-buffer (target::<fixnum> access::<ulong>) ::<void>
  (ENSURE glMapBuffer)
  (glMapBuffer target access))

(define-cproc gl-unmap-buffer (target::<int>) ::<boolean>
  (ENSURE glUnmapBuffer)
  (result (glUnmapBuffer target)))

(define-cproc gl-map-buffer-range (target::<fixnum> 
                                   offset::<fixnum>
                                   length::<ulong>
                                   access::<ulong>)
  ::<void>
  (ENSURE glMapBufferRange)
  (glMapBufferRange target offset length access))

(define-cproc gl-flush-mapped-buffer-range (target::<fixnum>
                                            offset::<fixnum>
                                            length::<fixnum>)
  ::<void>
  (ENSURE glFlushMappedBufferRange)
  (glFlushMappedBufferRange target offset length))

(define-cproc gl-copy-buffer-sub-data (readbuffer::<fixnum>
                                       writebuffer::<fixnum>
                                       readoffset::<fixnum>
                                       writeoffset::<fixnum>
                                       size::<fixnum>)
  ::<void>
  (ENSURE glCopyBufferSubData)
  (glCopyBufferSubData readbuffer writebuffer
                       readoffset writeoffset size))

;;=============================================================
;; GL_ARB_vertex_program
;;

(define-cproc gl-vertex-attrib-arb (index::<uint> arg0 &rest args) ::<void>
  (gl-case (arg0 args)
           (begin (ENSURE "glVertexAttrib~n~vARB")
                  ("glVertexAttrib~n~vARB" index ~X))
           ((p4f) (v4f) (f32 1 2 3 4) (s16 1 2 3 4) (f64 1 2 3 4)
            (s8 4) (u8 4) (u16 4) (s32 4) (u32 4) (args 1 2 3 4))
           "bad argument for gl-vertex-attrib-arb: %S"))

(define-cproc gl-vertex-attrib-4n-arb (index::<uint> arg0 &rest args) ::<void>
  (gl-case (arg0)
           (begin (ENSURE "glVertexAttrib4N~vARB")
                  ("glVertexAttrib4N~vARB" index ~X))
           ((s16 4) (s8 4) (u8 4) (u16 4) (s32 4) (u32 4))
           "bad argument for gl-vertex-attrib-4n-arb: %S"
           ;; We can't use 'args' as a fallback, for we need to coerce
           ;; arguments into GLubyte
           (cond [(== (Scm_Length args) 3)
                  (ENSURE glVertexAttrib4NubARB)
                  (glVertexAttrib4NubARB
                   index
                   (cast GLubyte (Scm_GetIntegerU arg0))
                   (cast GLubyte (Scm_GetIntegerU (SCM_CAR args)))
                   (cast GLubyte (Scm_GetIntegerU (SCM_CADR args)))
                   (cast GLubyte (Scm_GetIntegerU (SCM_CAR (SCM_CDDR args)))))]
                 [else
                  (Scm_Error "bad arguments for gl-vertex-attrib-4n-arb: %S"
                             (Scm_Cons arg0 args))])))

(define-cproc gl-vertex-attrib-pointer-arb (index::<uint>
                                            size::<int>
                                            vec
                                            &optional
                                            (normalized::<boolean> #f)
                                            (stride::<fixnum> 0)
                                            (offset::<fixnum> 0))
  ::<void>
  (unless (and (<= 1 size) (<= size 4))
    (Scm_Error "bad argument for size: %d, must be 1, 2, 3 or 4" size))
  (gl-case (vec)
           (begin
             (ENSURE glVertexAttribPointerARB)
             (glVertexAttribPointerARB index size ~E normalized stride
                                       (cast GLvoid* (+ ~X offset))))
           ((p4farray) (v4farray) (f32) (f64) (s32) (u32) (s16) (u16) (s8) (u8))
           "bad argument for vec: %S, must be an uniform vector, <pointer4f-array> or <vector4f-array>"))

(define-cproc gl-is-program-arb (prog-id::<int>) ::<boolean>
  (ENSURE glIsProgramARB)
  (result (glIsProgramARB prog_id)))

(define-cproc gl-enable-vertex-attrib-array-arb (index::<uint>) ::<void>
  (ENSURE glEnableVertexAttribArrayARB)
  (glEnableVertexAttribArrayARB index))

(define-cproc gl-disable-vertex-attrib-array-arb (index::<uint>) ::<void>
  (ENSURE glDisableVertexAttribArrayARB)
  (glDisableVertexAttribArrayARB index))

(define-cproc gl-program-string-arb (target::<int> format::<int>
                                                   text::<const-cstring>)
  ::<void>
  (let* ([errorPos::GLint])
    (ENSURE glProgramStringARB)
    (glProgramStringARB target format (strlen text) text)
    (glGetIntegerv GL_PROGRAM_ERROR_POSITION_ARB (& errorPos))
    (unless (== errorPos -1)
      (Scm_Error "Error in shader: %s"
                 (glGetString GL_PROGRAM_ERROR_STRING_ARB)))))

(define-cproc gl-bind-program-arb (target::<int> id::<int>) ::<void>
  (ENSURE glBindProgramARB)
  (glBindProgramARB target id))

(define-cproc gl-delete-programs-arb (arg0) ::<void>
  (cond [(SCM_INTEGERP arg0)
         (let* ([prog::GLuint (Scm_GetInteger arg0)])
           (ENSURE glDeleteProgramsARB)
           (glDeleteProgramsARB 1 (& prog)))]
        [(SCM_U32VECTORP arg0)
         (ENSURE glDeleteProgramsARB)
         (glDeleteProgramsARB (SCM_S32VECTOR_SIZE arg0)
                              (SCM_U32VECTOR_ELEMENTS arg0))]))

(define-cproc gl-gen-programs-arb (n::<int>)
  (let* ([v::ScmU32Vector* (SCM_U32VECTOR (Scm_MakeU32Vector n 0))])
    (ENSURE glGenProgramsARB)
    (glGenProgramsARB n (SCM_U32VECTOR_ELEMENTS v))
    (result (SCM_OBJ v))))

; target should be GL_FRAGMENT_PROGRAM_ARB or GL_VERTEX_PROGRAM_ARB
(define-cproc gl-program-env-parameter-arb 
  (target::<int> param-id::<int> arg0 &rest args) ::<void>
  (gl-case (arg0 args)
           (begin
             (ENSURE "glProgramEnvParameter~n~vARB")
             ("glProgramEnvParameter~n~vARB" target param_id ~X))
           ((v4f) (f32 4) (f64 4) (args 4))
           "bad argument for gl-program-env-parameter-arb: %S"))

; target should be GL_FRAGMENT_PROGRAM_ARB or GL_VERTEX_PROGRAM_ARB
(define-cproc gl-program-local-parameter-arb
  (target::<int> param-id::<int> arg0 &rest args) ::<void>
  (gl-case (arg0 args)
           (begin
             (ENSURE "glProgramLocalParameter~n~vARB")
             ("glProgramLocalParameter~n~vARB" target param_id ~X))
           ((v4f) (f32 4) (f64 4) (args 4))
           "vector4f, f32vector or f64vector of length 4 required, but got: %S"))
; gl-get-program-env-parameter-arb
; gl-get-program-local-parameter-arb
; gl-get-program-arb
; gl-get-program-string-arb
; gl-get-vertex-attrib-arb
; gl-get-vertex-attrib-pointer-arb

;;=============================================================
;; GL_ARB_vertex_shader
;;

(define-cproc gl-bind-attrib-location-arb (program::<gl-handle>
                                           index::<uint>
                                           name::<string>)
  ::<void>
  (ENSURE glBindAttribLocationARB)
  (glBindAttribLocationARB program index (Scm_GetStringConst name)))

;; NB: should be dynamically adjusted, using GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB
"#define MAXNAMEBUFLEN 1024"

;; returns three values: size, type and name
(define-cproc gl-get-active-attrib-arb (program::<gl-handle>
                                        index::<uint>)
  ::(<int> <int> <top>)
  (let* ([namebuf::(.array GLcharARB (MAXNAMEBUFLEN))]
         (attrsize::GLint 0)
         (attrtype::GLenum 0))
    (ENSURE glGetActiveAttribARB)
    (glGetActiveAttribARB program index (- MAXNAMEBUFLEN 1) NULL
                          (& attrsize) (& attrtype) namebuf)
    (result attrsize attrtype (SCM_MAKE_STR_COPYING namebuf))))

(define-cproc gl-get-attrib-location-arb (program::<gl-handle>
                                          name::<string>)
  ::<int>
  (ENSURE glGetAttribLocationARB)
  (result (glGetAttribLocationARB program (Scm_GetStringConst name))))

;;=============================================================
;; GL_ARB_window_pos
;;

(define-cproc gl-window-pos-arb (arg0 &rest args) ::<void>
  (gl-case (arg0 args) (begin (ENSURE "glWindowPos~n~vARB")
                              ("glWindowPos~n~vARB" ~X))
           ((f32 2 3) (s32 2 3) (s16 2 3) (f64 2 3) (args 2 3))
           "bad arguments for gl-window-pos: %S"))

;;=============================================================
;; GL_ATI_texture_float
;;

;;=============================================================
;; GL_ATI_draw_buffers
;;

(define-cproc gl-draw-buffers-ati (bufs::<s32vector>) ::<void>
  (ENSURE glDrawBuffersATI)
  (glDrawBuffersATI (SCM_S32VECTOR_SIZE bufs)
                    (cast (const GLenum*) (SCM_S32VECTOR_ELEMENTS bufs))))

;;=============================================================
;; GL_EXT_422_pixels
;;

;;=============================================================
;; GL_EXT_abgr
;;

;;=============================================================
;; GL_EXT_bgra
;;

;;=============================================================
;; GL_EXT_blend_color
;;

(define-cproc gl-blend-color-ext (red::<float> green::<float>
                                  blue::<float> alpha::<float>)
  ::<void>
  (ENSURE glBlendColorEXT)
  (glBlendColorEXT red green blue alpha))

;;=============================================================
;; GL_EXT_blend_equation_separate
;;

(define-cproc gl-blend-equation-separate-ext (mode-rgb::<int>
                                              mode-alpha::<int>)
  ::<void>
  (ENSURE glBlendEquationSeparateEXT)
  (glBlendEquationSeparateEXT mode_rgb mode_alpha))

;;=============================================================
;; GL_EXT_blend_func_separate
;;

(define-cproc gl-blend-func-separate-ext (sfactor-rgb::<int>
                                          dfactor-rgb::<int>
                                          sfactor-alpha::<int>
                                          dfactor-alpha::<int>)
  ::<void>
  (ENSURE glBlendFuncSeparateEXT)
  (glBlendFuncSeparateEXT sfactor_rgb dfactor_rgb sfactor_alpha dfactor_alpha))

;;=============================================================
;; GL_EXT_blend_logic_op
;;

;;=============================================================
;; GL_EXT_blend_minmax
;;

(define-cproc gl-blend-equation-ext (mode::<int>) ::<void>
  (ENSURE glBlendEquationEXT)
  (glBlendEquationEXT mode))

;;=============================================================
;; GL_EXT_blend_subtract
;;

;;=============================================================
;; GL_EXT_Cg_shader
;;

;;=============================================================
;; GL_EXT_polygon_offset
;;

;;=============================================================
;; GL_EXT_clip_volume_hint
;;

;;=============================================================
;; GL_EXT_cmyka
;;

;;=============================================================
;; GL_EXT_color_subtable
;;

;;=============================================================
;; GL_EXT_compiled_vertex_array
;;

(define-cproc gl-lock-arrays-ext (first::<int> count::<uint>) ::<void>
  (ENSURE glLockArraysEXT)
  (glLockArraysEXT first count))

(define-cproc gl-unlock-arrays-ext () ::<void>
  (ENSURE glUnlockArraysEXT)
  (glUnlockArraysEXT))

;;=============================================================
;; GL_EXT_convolution
;;

;;=============================================================
;; GL_EXT_copy_texture
;;

;;=============================================================
;; GL_EXT_cull_vertex
;;

;;=============================================================
;; GL_EXT_depth_bounds_test
;;

(define-cproc gl-depth-bounds-ext (zmin::<double> zmax::<double>) ::<void>
  (ENSURE glDepthBoundsEXT)
  (glDepthBoundsEXT zmin zmax))

;;=============================================================
;; GL_EXT_draw_range_elements
;;

; gl-draw-range-elements-ext

;;=============================================================
;; GL_EXT_fog_coord
;;

; gl-fog-coord-ext
; gl-fog-coord-pointer-ext

;;=============================================================
;; GL_EXT_histogram
;;

;;=============================================================
;; GL_EXT_misc_attribute
;;

;;=============================================================
;; GL_EXT_index_array_formats
;;

;;=============================================================
;; GL_EXT_index_func
;;

;;=============================================================
;; GL_EXT_index_material
;;

;;=============================================================
;; GL_EXT_index_texture
;;

;;=============================================================
;; GL_EXT_light_texture
;;

;;=============================================================
;; GL_EXT_multi_draw_arrays
;;

; gl-multi-draw-arrays-ext
; gl-multi-draw-elements-ext

;;=============================================================
;; GL_EXT_packed_pixels
;;

;;=============================================================
;; GL_EXT_paletted_texture
;;

; gl-color-table-ext
; gl-color-sub-table-ext
; gl-get-color-table-ext
; gl-get-color-table-parameter-ext

;;=============================================================
;; GL_EXT_pixel_buffer_object
;;

;;=============================================================
;; GL_EXT_pixel_transform
;;

;;=============================================================
;; GL_EXT_pixel_transform_color_table
;;

;;=============================================================
;; GL_EXT_point_parameters
;;

; gl-point-parameter-ext

;;=============================================================
;; GL_EXT_rescale_normal
;;

;;=============================================================
;; GL_EXT_secondary_color
;;

;;=============================================================
;; GL_EXT_separate_specular_color
;;

;;=============================================================
;; GL_EXT_shadow_funcs
;;

;;=============================================================
;; GL_EXT_shared_texture_palette
;;

;;=============================================================
;; GL_EXT_stencil_two_side
;;

;;=============================================================
;; GL_EXT_stencil_wrap
;;

;;=============================================================
;; GL_EXT_subtexture
;;

;;=============================================================
;; GL_EXT_texture
;;

;;=============================================================
;; GL_EXT_texture_compression_s3tc
;;

;;=============================================================
;; GL_EXT_texture_cube_map
;;

;;=============================================================
;; GL_EXT_coordinate_frame
;;

;;=============================================================
;; GL_EXT_texture_edge_clamp
;;

;;=============================================================
;; GL_EXT_texture_env_add
;;

;;=============================================================
;; GL_EXT_texture_env_combine
;;

;;=============================================================
;; GL_EXT_texture_env_dot3
;;

;;=============================================================
;; GL_EXT_texture_filter_anisotropic
;;

;;=============================================================
;; GL_EXT_texture_lod_bias
;;

;;=============================================================
;; GL_EXT_texture_object
;;

;;=============================================================
;; GL_EXT_texture_perturb_normal
;;

;;=============================================================
;; GL_EXT_texture3D
;;

;;=============================================================
;; GL_EXT_texture_rectangle
;;

;;=============================================================
;; GL_EXT_vertex_array
;;

(define-cproc gl-gen-vertex-arrays (size::<fixnum>)
  (let* ([v (Scm_MakeU32Vector size 0)])
    (glGenVertexArrays size (cast GLuint* (SCM_U32VECTOR_ELEMENTS v)))
    (return v)))

(define-cproc gl-bind-vertex-array (array_no::<fixnum>) ::<void>
  (glBindVertexArray array_no))

(define-cproc gl-delete-vertex-arrays (arrays::<u32vector>) ::<void>
  (glDeleteVertexArrays (SCM_U32VECTOR_SIZE arrays)
                        (cast GLuint* (SCM_U32VECTOR_ELEMENTS arrays))))

(define-cproc gl-is-vertex-array (array_no::<fixnum>) ::<boolean>
  (return (glIsVertexArray array_no)))

;;=============================================================
;; GL_EXT_vertex_weighting
;;

;;=============================================================
;; GL_NV_blend_square
;;

;;=============================================================
;; GL_NV_copy_depth_to_color
;;

;;=============================================================
;; GL_NV_depth_clamp
;;

;;=============================================================
;; GL_NV_element_array
;;

;;=============================================================
;; GL_NV_fence
;;

;;=============================================================
;; GL_NV_float_buffer
;;

;;=============================================================
;; GL_NV_fog_distance
;;

;;=============================================================
;; GL_NV_fragment_program
;;

;;=============================================================
;; GL_NV_fragment_program2
;;

;;=============================================================
;; GL_NV_half_float
;;

;;=============================================================
;; GL_NV_light_max_exponent
;;

;;=============================================================
;; GL_NV_multisample_filter_hint
;;

;;=============================================================
;; GL_NV_occlusion_query
;;

;;=============================================================
;; GL_NV_packed_depth_stencil
;;

;;=============================================================
;; GL_NV_pixel_buffer_object
;;

;;=============================================================
;; GL_NV_pixel_data_range
;;

;;=============================================================
;; GL_NV_point_sprite
;;

;;=============================================================
;; GL_NV_primitive_restart
;;

;;=============================================================
;; GL_NV_register_combiners
;;

;;=============================================================
;; GL_NV_register_combiners2
;;

;;=============================================================
;; GL_NV_stencil_two_side
;;

;;=============================================================
;; GL_NV_texgen_emboss
;;

;;=============================================================
;; GL_NV_texgen_reflection
;;

;;=============================================================
;; GL_NV_texture_compression_vtc
;;

;;=============================================================
;; GL_NV_texture_env_combine4
;;

;;=============================================================
;; GL_NV_texture_expand_normal
;;

;;=============================================================
;; GL_NV_texture_rectangle
;;

;;=============================================================
;; GL_NV_texture_shader
;;

;;=============================================================
;; GL_NV_texture_shader2
;;

;;=============================================================
;; GL_NV_texture_shader3
;;

;;=============================================================
;; GL_NV_vertex_array_range
;;

;;=============================================================
;; GL_NV_vertex_array_range2
;;

;;=============================================================
;; GL_NV_vertex_program
;;

;;=============================================================
;; GL_NV_vertex_program1_1
;;

;;=============================================================
;; GL_NV_vertex_program2
;;

;;=============================================================
;; GL_NV_vertex_program2_option
;;

;;=============================================================
;; GL_NV_vertex_program3
;;

;;=============================================================
;; GL_EXT_framebuffer_object
;;

(define-cproc gl-is-renderbuffer-ext (renderbuffer::<uint>) ::<boolean>
  (ENSURE glIsRenderbufferEXT)
  (result (glIsRenderbufferEXT renderbuffer)))

(define-cproc gl-bind-renderbuffer-ext (target::<int> renderbuffer::<uint>)
  ::<void>
  (ENSURE glBindRenderbufferEXT)
  (glBindRenderbufferEXT target renderbuffer))

(define-cproc gl-gen-renderbuffers-ext (size::<int>)
  (ENSURE glGenRenderbuffersEXT)
  (when (<= size 0)
    (Scm_Error "size must be a positive integer, but got %d" size))
  (let* ([vec (Scm_MakeU32Vector size 0)])
    (glGenRenderbuffersEXT size (cast GLuint* (SCM_U32VECTOR_ELEMENTS vec)))
    (result vec)))

(define-cproc gl-renderbuffer-storage-ext (target::<int>
                                           internalformat::<int>
                                           width::<uint> height::<uint>)
  ::<void>
  (ENSURE glRenderbufferStorageEXT)
  (glRenderbufferStorageEXT target internalformat width height))

(define-cproc gl-get-renderbuffer-parameter-ext (target::<int>
                                                 pname::<int>)
  (ENSURE glGetRenderbufferParameterivEXT)
  (case pname
    [(GL_RENDERBUFFER_WIDTH_EXT
      GL_RENDERBUFFER_HEIGHT_EXT
      GL_RENDERBUFFER_INTERNAL_FORMAT_EXT
      GL_RENDERBUFFER_RED_SIZE_EXT
      GL_RENDERBUFFER_GREEN_SIZE_EXT
      GL_RENDERBUFFER_BLUE_SIZE_EXT
      GL_RENDERBUFFER_ALPHA_SIZE_EXT
      GL_RENDERBUFFER_DEPTH_SIZE_EXT
      GL_RENDERBUFFER_STENCIL_SIZE_EXT)
     (let* ([val::GLint])
       (glGetRenderbufferParameterivEXT target pname (& val))
       (result (Scm_MakeInteger val)))]
    [else
     (Scm_Error "unsupported pname for gl-get-renderbuffer-parameter-ext: %S"
                pname)]))

(define-cproc gl-bind-framebuffer-ext (target::<int> framebuffer::<uint>)
  ::<void>
  (ENSURE glBindFramebufferEXT)
  (glBindFramebufferEXT target framebuffer))

(define-cproc gl-delete-framebuffers-ext (fbs::<u32vector>) ::<void>
  (ENSURE glDeleteFramebuffersEXT)
  (glDeleteFramebuffersEXT (SCM_U32VECTOR_SIZE fbs)
                           (cast GLuint* (SCM_U32VECTOR_ELEMENTS fbs))))
       
(define-cproc gl-gen-framebuffers-ext (size::<int>)
  (ENSURE glGenFramebuffersEXT)
  (when (<= size 0)
    (Scm_Error "size must be a positive integer, but got %d" size))
  (let* ([vec (Scm_MakeU32Vector size 0)])
    (glGenFramebuffersEXT size (cast GLuint* (SCM_U32VECTOR_ELEMENTS vec)))
    (result vec)))

(define-cproc gl-check-framebuffer-status-ext (target::<int>) ::<int>
  (ENSURE glCheckFramebufferStatusEXT)
  (result (glCheckFramebufferStatusEXT target)))

(define-cproc gl-framebuffer-texture-1d-ext (target::<int>
                                             attachment::<int>
                                             textarget::<int>
                                             texture::<uint>
                                             level::<int>)
  ::<void>
  (ENSURE glFramebufferTexture1DEXT)
  (glFramebufferTexture1DEXT target attachment textarget texture level))
  
(define-cproc gl-framebuffer-texture-2d-ext (target::<int>
                                             attachment::<int>
                                             textarget::<int>
                                             texture::<uint>
                                             level::<int>)
  ::<void>
  (ENSURE glFramebufferTexture2DEXT)
  (glFramebufferTexture2DEXT target attachment textarget texture level))
  
(define-cproc gl-framebuffer-texture-3d-ext (target::<int>
                                             attachment::<int>
                                             textarget::<int>
                                             texture::<uint>
                                             level::<int>
                                             zoffset::<int>)
  ::<void>
  (ENSURE glFramebufferTexture3DEXT)
  (glFramebufferTexture3DEXT target attachment textarget texture
                             level zoffset))
  
(define-cproc gl-framebuffer-renderbuffer-ext (target::<int>
                                               attachment::<int>
                                               renderbuffertarget::<int>
                                               renderbuffer::<uint>)
  ::<void>
  (ENSURE glFramebufferRenderbufferEXT)
  (glFramebufferRenderbufferEXT target attachment
                                renderbuffertarget renderbuffer))

(define-cproc gl-get-framebuffer-attachment-parameter-ext (target::<int>
                                                           attachment::<int>
                                                           pname::<int>)
  (ENSURE glGetFramebufferAttachmentParameterivEXT)
  (case pname
    [(GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT
      GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT
      GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT
      GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT
      GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT)
     (let* ([val::GLint])
       (glGetFramebufferAttachmentParameterivEXT target attachment pname
                                                 (& val))
       (result (Scm_MakeInteger val)))]
    [else
     (Scm_Error "unsupported pname for gl-get-renderbuffer-parameter-ext: %S"
                pname)]))

(define-cproc gl-generate-mipmap-ext (target::<int>) ::<void>
  (ENSURE glGenerateMipmapEXT)
  (glGenerateMipmapEXT target))

) ;; end inline-stub

;; Backward compatibility names
;; We define them to not break old code.

(define gl-bind-buffer-arb gl-bind-buffer)
(define gl-delete-buffers-arb gl-delete-buffers)
(define gl-gen-buffers-arb gl-gen-buffers)
(define gl-is-buffer-arb gl-is-buffer)
(define gl-unmap-buffer-arb gl-unmap-buffer)

;; Local variables:
;; mode: scheme
;; end:
