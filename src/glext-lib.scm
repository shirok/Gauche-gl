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
 (declcode (.include "gauche-gl.h"
                     "gl-syms.h"
                     "gl-ptrs.h"))
 (include "glcase.scm")
 )

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
  (return (SCM_OBJ data)))

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
      (return vec))))

(define-cproc gl-get-histogram-parameter (target::<fixnum>
                                          pname::<fixnum>)
  (ENSURE glGetHistogramParameteriv)
  (let* ([param::GLint])
    (case pname
      [(GL_HISTOGRAM_SINK)
       (glGetHistogramParameteriv target pname (& param))
       (return (SCM_MAKE_BOOL param))]
      [else
       (glGetHistogramParameteriv target pname (& param))
       (return (Scm_MakeInteger param))])))

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
    (return vec)))

(define-cproc gl-get-minmax-parameter (target::<fixnum>
                                       pname::<fixnum>)
  (ENSURE glGetMinmaxParameteriv)
  (let* ([param::GLint])
    (case pname
      [(GL_MINMAX_SINK)
       (glGetMinmaxParameteriv target pname (& param))
       (return (SCM_MAKE_BOOL param))]
      [else
       (glGetMinmaxParameteriv target pname (& param))
       (return (Scm_MakeInteger param))])))

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

(inline-stub
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
 )

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
       (return (Scm_MakeInteger r)))]
    [(GL_CONVOLUTION_BORDER_COLOR
      GL_CONVOLUTION_FILTER_SCALE
      GL_CONVOLUTION_FILTER_BIAS)
     (ENSURE glGetConvolutionParameterfv)
     (let* ([v (Scm_MakeF32Vector 4 0)])
       (glGetConvolutionParameterfv target pname (SCM_F32VECTOR_ELEMENTS v))
       (return v))]
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

(define-cproc gl-sample-coverage (value::<float> invert::<boolean>)
  ::<void>
  (ENSURE glSampleCoverage)
  (glSampleCoverage value invert))

;;=============================================================
;; GL_ARB_multitexture
;;

(define-cproc gl-active-texture (texture::<int>) ::<void> ; GL 1.3
  (ENSURE glActiveTexture)
  (glActiveTexture texture))

(define-cproc gl-client-active-texture (texture::<int>) ::<void>; GL 1.3
  (ENSURE glClientActiveTexture)
  (glClientActiveTexture texture))

(define-cproc gl-multi-tex-coord (texunit::<int> v &rest args) ::<void>
  (gl-case (v args)
           (begin
             (ENSURE "glMultiTexCoord~n~v")
             ("glMultiTexCoord~n~v" texunit ~X))
           ((f32 2 1 3 4) (f64 2 1 3 4) (s32 2 1 3 4) (s16 2 1 3 4)
            (args 2 1 3 4))
           "bad argument for v: %S, must be one of f32, f64, s32 or s16 vector of length 1, 2, 3, or 4."))

;;=============================================================
;; GL_ARB_occlusion_query
;;

;; gl-genqueries-arb
;; gl-delete-queries-arb

(define-cproc gl-is-query (query::<uint>) ::<boolean>
  (ENSURE glIsQuery)
  (return (glIsQuery query)))

(define-cproc gl-begin-query (op::<uint> query::<uint>) ::<void>
  (ENSURE glBeginQuery)
  (glBeginQuery op query))

(define-cproc gl-end-query (op::<uint>) ::<void>
  (ENSURE glEndQuery)
  (glEndQuery op))

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

(define-cproc gl-delete-shader (shader::<uint>) ::<void>
  (ENSURE glDeleteShader)
  (glDeleteShader shader))

(define-cproc gl-create-shader (type::<uint>) ::<uint>
  (ENSURE glCreateShader)
  (return (glCreateShader type)))

(define-cproc gl-shader-source (shader::<uint> strings) ::<void>
  (let* ([nstrings::GLint (Scm_Length strings)]
         [i::int 0])
    (ENSURE glShaderSource)
    (when (< nstrings 0)
      (label einval)
      (Scm_Error "list of strings required, but got %S" strings))
    (let* ([lengths::GLint*
            (SCM_NEW_ATOMIC2 (GLint*) (* nstrings (sizeof GLint)))]
           ;; NB: we can use atomic here, since all strings are pointed by the
           ;; input parameter, and we don't need this array after calling
           ;; glShaderSource.
           [ss::GLchar**
            (SCM_NEW_ATOMIC2 (GLchar**) (* nstrings (sizeof (GLchar*))))])
      (dolist [s strings]
        (unless (SCM_STRINGP s) (goto einval))
        (set! (aref lengths i) (SCM_STRING_SIZE s))
        (set! (aref ss i) (cast GLchar* (SCM_STRING_START s)))
        (inc! i))
      (glShaderSource shader nstrings (cast (const GLchar**) ss)
                      lengths))))

(define-cproc gl-compile-shader (shader::<uint>) ::<void>
  (ENSURE glCompileShader)
  (glCompileShader shader))

(define-cproc gl-delete-program (program::<uint>) ::<void>
  (ENSURE glDeleteProgram)
  (glDeleteProgram program))

(define-cproc gl-create-program () ::<uint>
  (ENSURE glCreateProgram)
  (return (glCreateProgram)))

(define-cproc gl-attach-shader (program::<uint> shader::<uint>)
  ::<void>
  (ENSURE glAttachShader)
  (glAttachShader program shader))

(define-cproc gl-detach-shader (program::<uint> shader::<uint>)
  ::<void>
  (ENSURE glDetachShader)
  (glDetachShader program shader))

(define-cproc gl-link-program (program::<uint>) ::<void>
  (ENSURE glLinkProgram)
  (glLinkProgram program))

(define-cproc gl-use-program (program::<uint>) ::<void>
  (ENSURE glUseProgram)
  (glUseProgram program))

(define-cproc gl-validate-program (program::<uint>) ::<void>
  (ENSURE glValidateProgram)
  (glValidateProgram program))

(define-cproc gl-uniform1 (location::<int> v0) ::<void>
  (cond
   [(SCM_F32VECTORP v0)
    (let* ([count::int (SCM_F32VECTOR_SIZE v0)])
      (ENSURE glUniform1fv)
      (glUniform1fv location count (SCM_F32VECTOR_ELEMENTS v0)))]
   [(SCM_S32VECTORP v0)
    (let* ([count::int (SCM_S32VECTOR_SIZE v0)])
      (ENSURE glUniform1iv)
      (glUniform1iv location count (SCM_S32VECTOR_ELEMENTS v0)))]
   [else
    (ENSURE glUniform1f)
    (glUniform1f location (cast GLfloat (Scm_GetDouble v0)))]))

(define-cproc gl-uniform2 (location::<int> v0 &optional v1) ::<void>
  (cond
   [(SCM_F32VECTORP v0)
    (let* ([count::int (/ (SCM_F32VECTOR_SIZE v0) 2)])
      (ENSURE glUniform2fv)
      (glUniform2fv location count (SCM_F32VECTOR_ELEMENTS v0)))]
   [(SCM_S32VECTORP v0)
    (let* ([count::int (/ (SCM_S32VECTOR_SIZE v0) 2)])
      (ENSURE glUniform2iv)
      (glUniform2iv location count (SCM_S32VECTOR_ELEMENTS v0)))]
   [(SCM_UNBOUNDP v1)
    (Scm_Error "Not enough arguments for gl-uniform2")]
   [else
    (ENSURE glUniform2f)
    (glUniform2f location
                 (cast GLfloat (Scm_GetDouble v0))
                 (cast GLfloat (Scm_GetDouble v1)))]))

(define-cproc gl-uniform3 (location::<int> v0 &optional v1 v2) ::<void>
  (cond
   [(SCM_F32VECTORP v0)
    (let* ([count::int (/ (SCM_F32VECTOR_SIZE v0) 3)])
      (ENSURE glUniform3fv)
      (glUniform3fv location count (SCM_F32VECTOR_ELEMENTS v0)))]
   [(SCM_S32VECTORP v0)
    (let* ([count::int (/ (SCM_S32VECTOR_SIZE v0) 3)])
      (ENSURE glUniform3iv)
      (glUniform3iv location count (SCM_S32VECTOR_ELEMENTS v0)))]
   [(SCM_UNBOUNDP v2)
    (Scm_Error "Not enough arguments for gl-uniform3")]
   [else
    (ENSURE glUniform3f)
    (glUniform3f location (cast GLfloat (Scm_GetDouble v0))
                 (cast GLfloat (Scm_GetDouble v1))
                 (cast GLfloat (Scm_GetDouble v2)))]))

(define-cproc gl-uniform4 (location::<int> v0 &optional v1 v2 v3) ::<void>
  (cond
   [(SCM_F32VECTORP v0)
    (let* ([count::int (/ (SCM_F32VECTOR_SIZE v0) 4)])
      (ENSURE glUniform4fv)
      (glUniform4fv location count (SCM_F32VECTOR_ELEMENTS v0)))]
   [(SCM_S32VECTORP v0)
    (let* ([count::int (/ (SCM_S32VECTOR_SIZE v0) 4)])
      (ENSURE glUniform4iv)
      (glUniform4iv location count (SCM_S32VECTOR_ELEMENTS v0)))]
   [(SCM_UNBOUNDP v3)
    (Scm_Error "Not enough arguments for gl-uniform4")]
   [else
    (ENSURE glUniform4f)
    (glUniform4f location (cast GLfloat (Scm_GetDouble v0))
                 (cast GLfloat (Scm_GetDouble v1))
                 (cast GLfloat (Scm_GetDouble v2))
                 (cast GLfloat (Scm_GetDouble v3)))]))

(define-cproc gl-uniform-matrix2 (location::<int>
                                  transpose::<boolean>
                                  v::<f32vector>)
  ::<void>
  (let* ([count::int (/ (SCM_F32VECTOR_SIZE v) 4)])
    (ENSURE glUniformMatrix2fv)
    (glUniformMatrix2fv location count transpose
                        (SCM_F32VECTOR_ELEMENTS v))))

(define-cproc gl-uniform-matrix3 (location::<int>
                                  transpose::<boolean>
                                  v::<f32vector>)
  ::<void>
  (let* ([count::int (/ (SCM_F32VECTOR_SIZE v) 9)])
    (ENSURE glUniformMatrix3fv)
    (glUniformMatrix3fv location count transpose
                        (SCM_F32VECTOR_ELEMENTS v))))

(define-cproc gl-uniform-matrix4 (location::<int>
                                  transpose::<boolean>
                                  v::<f32vector>)
  ::<void>
  (let* ([count::int (/ (SCM_F32VECTOR_SIZE v) 16)])
    (ENSURE glUniformMatrix4fv)
    (glUniformMatrix4fv location count transpose
                        (SCM_F32VECTOR_ELEMENTS v))))

(define-cproc gl-get-shader (shader::<uint>
                             pname::<uint>)
 (ENSURE glGetShaderiv)
 (case pname
   [(GL_SHADER_TYPE
     GL_DELETE_STATUS
     GL_COMPILE_STATUS
     GL_INFO_LOG_LENGTH
     GL_SHADER_SOURCE_LENGTH)
    (let* ([i::GLint])
      (glGetShaderiv shader pname (& i))
      (return (Scm_MakeInteger i)))]
   [else
    (Scm_Error "invalid pname for gl-get-shader: %d" pname)]))

(define-cproc gl-get-program (program::<uint>
                              pname::<uint>)
 (ENSURE glGetProgramiv)
 (case pname
   [(GL_DELETE_STATUS
     GL_LINK_STATUS
     GL_VALIDATE_STATUS
     GL_INFO_LOG_LENGTH
     GL_ATTACHED_SHADERS
     GL_ACTIVE_ATOMIC_COUNTER_BUFFERS
     GL_ACTIVE_ATTRIBUTES
     GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
     GL_ACTIVE_UNIFORMS
     GL_ACTIVE_UNIFORM_MAX_LENGTH
     GL_PROGRAM_BINARY_LENGTH
     GL_COMPUTE_WORK_GROUP_SIZE
     GL_TRANSFORM_FEEDBACK_BUFFER_MODE
     GL_TRANSFORM_FEEDBACK_VARYINGS
     GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
     GL_GEOMETRY_VERTICES_OUT
     GL_GEOMETRY_INPUT_TYPE
     GL_GEOMETRY_OUTPUT_TYPE)
    (let* ([i::GLint])
      (glGetProgramiv program pname (& i))
      (return (Scm_MakeInteger i)))]
   [else
    (Scm_Error "invalid pname for gl-get-program: %d" pname)]))

(define-cproc gl-get-shader-info-log (shader::<uint>
                                      :optional (max-length::<fixnum> 0))
  (let* ([loglen::GLsizei max-length])
    (ENSURE glGetShaderInfoLog)
    (ENSURE glGetShaderiv)
    (when (== loglen 0)
      (let* ([param::GLint 0])
        (glGetShaderiv shader GL_INFO_LOG_LENGTH (& param))
        (CHECK_ERROR "glGetShaderiv")
        (set! loglen param)))
    (let* ([buf::char* (SCM_NEW_ATOMIC_ARRAY (char) (+ loglen 1))])
      (glGetShaderInfoLog shader loglen (& loglen) buf)
      (CHECK_ERROR "glGetShaderInfoLog")
      (return (Scm_MakeString buf loglen -1 0)))))

(define-cproc gl-get-program-info-log (program::<uint>
                                       :optional (max-length::<fixnum> 0))
  (let* ([loglen::GLsizei max-length])
    (ENSURE glGetProgramInfoLog)
    (ENSURE glGetProgramiv)
    (when (== loglen 0)
      (let* ([param::GLint 0])
        (glGetProgramiv program GL_INFO_LOG_LENGTH (& param))
        (CHECK_ERROR "glGetProgramiv")
        (set! loglen param)))
    (let* ([buf::char* (SCM_NEW_ATOMIC_ARRAY (.type char) (+ loglen 1))])
      (glGetProgramInfoLog program loglen (& loglen) buf)
      (CHECK_ERROR "glGetProgramInfoLog")
      (return (Scm_MakeString buf loglen -1 0)))))

(define-cproc gl-get-attached-shaders (program::<uint>
                                       :optional (max-count::<fixnum> 0))
  (let* ([count::GLsizei max-count])
    (ENSURE glGetAttachedShaders)
    (ENSURE glGetProgramiv)
    (when (== count 0)
      (let* ([param::GLint 0])
        (glGetProgramiv program GL_ATTACHED_SHADERS (& param))
        (CHECK_ERROR "glGetProgramiv")
        (set! count param)))
    (let* ([buf::GLuint* (SCM_NEW_ATOMIC_ARRAY (.type GLuint) count)])
      (glGetAttachedShaders program count (& count) buf)
      (CHECK_ERROR "glGetAttachedShaders")
      (return (Scm_MakeU32VectorFromArray count buf)))))

(define-cproc gl-get-uniform-location (program::<uint>
                                       name::<const-cstring>)
  ::<fixnum>
  (ENSURE glGetUniformLocation)
  (let* ([r::GLint (glGetUniformLocation program (cast (const GLchar*) name))])
    (CHECK_ERROR "glGetUniformLocation")
    (return r)))

;; returns (size, type, name)
(define-cproc gl-get-active-uniform (program::<uint>
                                     index::<uint>)
  ::(<int> <int> <top>)
  (ENSURE glGetProgramiv)
  (ENSURE glGetActiveUniform)
  (let* ([maxlen::GLint 0])
    (glGetProgramiv program GL_ACTIVE_UNIFORM_MAX_LENGTH (& maxlen))
    (CHECK_ERROR "glGetProgramiv")
    (when (< maxlen 0) (= maxlen 0))
    (let* ([len::GLsizei] [size::GLint] [type::GLenum]
           [namebuf::GLchar* (SCM_NEW_ATOMIC2 (.type GLchar*)
                                              (* (+ maxlen 1)
                                                 (sizeof GLchar)))])
      (glGetActiveUniform program index maxlen
                          (& len) (& size) (& type) namebuf)
      (CHECK_ERROR "glGetActiveUniform")
      (set! (aref namebuf maxlen) 0)
      (return size type (Scm_MakeString (cast (const char*) namebuf) len -1 0)))))

;; NB: It's not easy to obtain the size of uniform, so we ask the caller
;; to provide a buffer to store the result.
(define-cproc gl-get-uniform! (program::<uint>
                               location::<int>
                               result::<uvector>)
  (gl-case (result)
           (begin
             (ENSURE "glGetUniform~v")
             ("glGetUniform~v" program location ~X))
           ((f32) (f64) (s32) (u32))
           "f32vector or s32vector is required to store the result, but got %S"))

(define-cproc gl-get-shader-source (shader::<uint>)
  (ENSURE glGetShaderiv)
  (ENSURE glGetShaderSource)
  (let* ([srclen::GLint 0])
    (glGetShaderiv shader GL_SHADER_SOURCE_LENGTH (& srclen))
    (CHECK_ERROR "glGetShaderiv")
    (when (< srclen 0) (= srclen 0))
    (let* ([srcstr::GLchar* (SCM_NEW_ATOMIC_ARRAY (.type GLchar) (+ srclen 1))])
      (glGetShaderSource shader srclen NULL srcstr)
      (CHECK_ERROR "glGetShadeSource")
      (= (aref srcstr srclen) 0)
      (return (Scm_MakeString (cast (const char*) srcstr) (- srclen 1) -1 0)))))



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

(define-cproc gl-load-transpose-matrix (m) ::<void>
  (gl-case (m) (begin (ENSURE "glLoadTransposeMatrix~t")
                      ("glLoadTransposeMatrix~t" ~X))
           ((m4f) (f32 16) (f64 16))
           "matrix4f, f32vector or f64vector of length 16 is required, but got %S"))

(define-cproc gl-mult-transpose-matrix (m) ::<void>
  (gl-case (m) (begin (ENSURE "glMultTransposeMatrix~t")
                      ("glMultTransposeMatrix~t" ~X))
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
    (return v)))

(define-cproc gl-is-buffer (buffer::<uint>) ::<boolean>
  (ENSURE glIsBuffer)
  (return (glIsBuffer buffer)))

;; NB: We need size argument, for data can be #f (NULL).   You can pass
;; 0 to size to make gl-buffer-data calculate one from data.
(define-cproc gl-buffer-data (target::<fixnum>
                              size::<ulong>
                              data
                              usage::<fixnum>)
  ::<void>
  (let* ([p::GLvoid* NULL]
         [isize::GLsizeiptr 0])
    (cond [(SCM_UVECTORP data)
           (set! p (SCM_UVECTOR_ELEMENTS data))
           (set! isize (Scm_UVectorSizeInBytes (SCM_UVECTOR data)))]
          [(SCM_POINT4F_ARRAY_P data)
           (set! p (SCM_POINT4F_ARRAY_D data))
           (set! isize (* (-> (SCM_POINT4F_ARRAY data) size)
                          (sizeof (float))
                          4))]
          [(SCM_FALSEP data)]
          [else
           (Scm_Error "data must be either uvector, point4f-array or #f, \
                       but got %S" data)])
    (when (== size 0)
      (if (== isize 0)
        (Scm_Error "You have to specify non-zero size parameter if you pass #f to data")
        (set! size isize)))
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
  (return (glUnmapBuffer target)))

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

(define-cproc gl-vertex-attrib (index::<uint> arg0 &rest args) ::<void>
  (gl-case (arg0 args)
           (begin (ENSURE "glVertexAttrib~n~v")
                  ("glVertexAttrib~n~v" index ~X))
           ((p4f) (v4f) (f32 1 2 3 4) (s16 1 2 3 4) (f64 1 2 3 4)
            (s8 4) (u8 4) (u16 4) (s32 4) (u32 4) (args 1 2 3 4))
           "bad argument for gl-vertex-attrib: %S"))

(define-cproc gl-vertex-attrib-4n (index::<uint> arg0 &rest args) ::<void>
  (gl-case (arg0)
           (begin (ENSURE "glVertexAttrib4N~v")
                  ("glVertexAttrib4N~v" index ~X))
           ((s16 4) (s8 4) (u8 4) (u16 4) (s32 4) (u32 4))
           "bad argument for gl-vertex-attrib-4n: %S"
           ;; We can't use 'args' as a fallback, for we need to coerce
           ;; arguments into GLubyte
           (cond [(== (Scm_Length args) 3)
                  (ENSURE glVertexAttrib4Nub)
                  (glVertexAttrib4Nub
                   index
                   (cast GLubyte (Scm_GetIntegerU arg0))
                   (cast GLubyte (Scm_GetIntegerU (SCM_CAR args)))
                   (cast GLubyte (Scm_GetIntegerU (SCM_CADR args)))
                   (cast GLubyte (Scm_GetIntegerU (SCM_CAR (SCM_CDDR args)))))]
                 [else
                  (Scm_Error "bad arguments for gl-vertex-attrib-4n: %S"
                             (Scm_Cons arg0 args))])))

;; NB: stride and offset are in bytes, to allow
(define-cproc gl-vertex-attrib-pointer (index::<uint>
                                        size::<int>
                                        type::<int>
                                        &optional
                                        (normalized::<boolean> #f)
                                        (stride::<fixnum> 0)
                                        (offset::<fixnum> 0))
  ::<void>
  (unless (and (<= 1 size) (<= size 4))
    (Scm_Error "bad argument for size: %d, must be 1, 2, 3 or 4" size))
  (ENSURE glVertexAttribPointer)
  (glVertexAttribPointer index size type (?: normalized GL_TRUE GL_FALSE) stride
                         (cast GLvoid* offset)))

(define-cproc gl-is-program (program::<uint>) ::<boolean>
  (ENSURE glIsProgram)
  (return (glIsProgram program)))

(define-cproc gl-enable-vertex-attrib-array (index::<uint>) ::<void>
  (ENSURE glEnableVertexAttribArray)
  (glEnableVertexAttribArray index))

(define-cproc gl-disable-vertex-attrib-array (index::<uint>) ::<void>
  (ENSURE glDisableVertexAttribArray)
  (glDisableVertexAttribArray index))

;;=============================================================
;; GL_ARB_vertex_shader
;;

(define-cproc gl-bind-attrib-location (program::<uint>
                                       index::<uint>
                                       name::<const-cstring>)
  ::<void>
  (ENSURE glBindAttribLocation)
  (glBindAttribLocation program index name))

;; NB: should be dynamically adjusted, using GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB
(inline-stub
 (.define MAXNAMEBUFLEN 1024))

;; returns three values: size, type and name
(define-cproc gl-get-active-attrib (program::<uint>
                                    index::<uint>)
  ::(<int> <int> <top>)
  (let* ([namebuf::(.array GLchar (MAXNAMEBUFLEN))]
         (attrsize::GLint 0)
         (attrtype::GLenum 0))
    (ENSURE glGetActiveAttrib)
    (glGetActiveAttrib program index (- MAXNAMEBUFLEN 1) NULL
                       (& attrsize) (& attrtype) namebuf)
    (return attrsize attrtype (SCM_MAKE_STR_COPYING namebuf))))

(define-cproc gl-get-attrib-location (program::<uint>
                                      name::<const-cstring>)
  ::<int>
  (ENSURE glGetAttribLocation)
  (return (glGetAttribLocation program name)))

;;=============================================================
;; GL_ARB_window_pos
;;

(define-cproc gl-window-pos (arg0 &rest args) ::<void>
  (gl-case (arg0 args) (begin (ENSURE "glWindowPos~n~v")
                              ("glWindowPos~n~v" ~X))
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
  (ENSURE glGenVertexArrays)
  (let* ([v (Scm_MakeU32Vector size 0)])
    (glGenVertexArrays size (cast GLuint* (SCM_U32VECTOR_ELEMENTS v)))
    (return v)))

(define-cproc gl-bind-vertex-array (array_no::<fixnum>) ::<void>
  (ENSURE glBindVertexArray)
  (glBindVertexArray array_no))

(define-cproc gl-delete-vertex-arrays (arrays::<u32vector>) ::<void>
  (ENSURE glDeleteVertexArrays)
  (glDeleteVertexArrays (SCM_U32VECTOR_SIZE arrays)
                        (cast GLuint* (SCM_U32VECTOR_ELEMENTS arrays))))

(define-cproc gl-is-vertex-array (array_no::<fixnum>) ::<boolean>
  (ENSURE glIsVertexArray)
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
  (return (glIsRenderbufferEXT renderbuffer)))

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
    (return vec)))

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
       (return (Scm_MakeInteger val)))]
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
    (return vec)))

(define-cproc gl-check-framebuffer-status-ext (target::<int>) ::<int>
  (ENSURE glCheckFramebufferStatusEXT)
  (return (glCheckFramebufferStatusEXT target)))

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
       (return (Scm_MakeInteger val)))]
    [else
     (Scm_Error "unsupported pname for gl-get-renderbuffer-parameter-ext: %S"
                pname)]))

(define-cproc gl-generate-mipmap-ext (target::<int>) ::<void>
  (ENSURE glGenerateMipmapEXT)
  (glGenerateMipmapEXT target))

;; Local variables:
;; mode: scheme
;; end:
