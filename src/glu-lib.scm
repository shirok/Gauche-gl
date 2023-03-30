;;;
;;; glu-lib.scm - glue functions for GLU
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
 (declcode (.include <gauche/uvector.h>
                     "gauche-gl.h"))

 (include "glcase.scm")
 )

;;================================================================
;; Miscellaneous
;;

(define-cproc glu-look-at (eyex::<double> eyey::<double> eyez::<double>
                           ctrx::<double> ctry::<double> ctrz::<double>
                           upx::<double> upy::<double> upz::<double>)
  ::<void> gluLookAt)

(define-cproc glu-ortho-2d (left::<double> right::<double>
                            bottom::<double> top::<double>)
  ::<void> gluOrtho2D)

(define-cproc glu-perspective (fovy::<double> aspect::<double>
                               znear::<double> zfar::<double>)
  ::<void> gluPerspective)

(define-cproc glu-pick-matrix (x::<double> y::<double> w::<double> h::<double> vp)
  ::<void> (gl-case (vp) (gluPickMatrix x y w h ~X) ((s32 4))
                    "s32 vector of size 4 required for vp, but got %S"))

(define-cproc glu-error-string (code::<int>)
  (let* ([s::(const GLubyte*) (gluErrorString code)])
    (if s
      (return (Scm_MakeString (cast (const char*) s) -1 -1 0))
      (return SCM_FALSE))))

;; common routine for glu-project and glu-un-project
(inline-stub
 (define-cise-stmt glu-xproject
   [(_ fn srcx srcy srcz)
    `(let* ([dstx::GLdouble] [dsty::GLdouble] [dstz::GLdouble])
       (assert-vector-type&size f64 16 model-mat)
       (assert-vector-type&size f64 16 proj-mat)
       (assert-vector-type&size s32 4 vp)
       (if (== GL_TRUE (,fn ,srcx ,srcy ,srcz
                            (SCM_F64VECTOR_ELEMENTS model-mat)
                            (SCM_F64VECTOR_ELEMENTS proj-mat)
                            (cast GLint* (SCM_S32VECTOR_ELEMENTS vp))
                            (& dstx) (& dsty) (& dstz)))
         (return (Scm_MakeFlonum dstx)(Scm_MakeFlonum dsty)(Scm_MakeFlonum dstz))
         (return '#f '#f '#f)))])
 )

(define-cproc glu-project (objx::<double> objy::<double> objz::<double>
                           model-mat proj-mat vp)
  ::(<top> <top> <top>)
  (glu-xproject gluProject objx objy objz))

(define-cproc glu-un-project (winx::<double> winy::<double> winz::<double>
                              model-mat proj-mat vp)
  ::(<top> <top> <top>)
  (glu-xproject gluUnProject winx winy winz))

;; common routine for glu-project! and glu-un-project!
(inline-stub
 (define-cise-stmt glu-xproject!
   [(_ fn dst src)
    `(let* ([dstfp::float* NULL] [srcfp::float* NULL]
            [dstdp::double* NULL] [srcdp::double* NULL]
            [dstx::GLdouble] [dsty::GLdouble] [dstz::GLdouble])
       ;; We allow f64 vector as well as float vector-likes (f32, point4f,
       ;; vector4f, quatf), so it's a bit complicated here.
       (if (and (SCM_F64VECTORP ,dst) (== (SCM_F64VECTOR_SIZE ,dst) 4))
         (set! dstdp (SCM_F64VECTOR_ELEMENTS ,dst))
         (SCM_MATH3D_X4FP dstfp ,dst))
       (if (and (SCM_F64VECTORP ,src) (== (SCM_F64VECTOR_SIZE ,src) 4))
         (set! srcdp (SCM_F64VECTOR_ELEMENTS ,src))
         (SCM_MATH3D_X4FP srcfp ,src))
       (assert-vector-type&size f64 16 model-mat)
       (assert-vector-type&size f64 16 proj-mat)
       (assert-vector-type&size s32 4 vp)
       (if (== GL_TRUE
               (?: srcfp
                   (,fn (aref srcfp 0) (aref srcfp 1) (aref srcfp 2)
                        (SCM_F64VECTOR_ELEMENTS model-mat)
                        (SCM_F64VECTOR_ELEMENTS proj-mat)
                        (cast GLint* (SCM_S32VECTOR_ELEMENTS vp))
                        (& dstx) (& dsty) (& dstz))
                   (,fn (aref srcdp 0) (aref srcdp 1) (aref srcdp 2)
                        (SCM_F64VECTOR_ELEMENTS model-mat)
                        (SCM_F64VECTOR_ELEMENTS proj-mat)
                        (cast GLint* (SCM_S32VECTOR_ELEMENTS vp))
                        (& dstx) (& dsty) (& dstz))))
         (begin
           (if dstfp
             (set! (aref dstfp 0) dstx
                   (aref dstfp 1) dsty
                   (aref dstfp 2) dstz)
             (set! (aref dstdp 0) dstx
                   (aref dstdp 1) dsty
                   (aref dstdp 2) dstz))
           (return (SCM_OBJ ,dst)))
         (return '#f)))])
 )

(define-cproc glu-project! (win obj model-mat proj-mat vp)
  (glu-xproject! gluProject win obj))

(define-cproc glu-un-project! (obj win model-mat proj-mat vp)
  (glu-xproject! gluUnProject obj win))

;;=============================================================
;; Mipmapping and image scaling
;;

;(define-cproc glu-scale-image! (format::<fixnum>
;                                width-in::<fixnum> height-in::<fixnum>
;                                type-in::<fixnum> data-in::<uvector>
;                                width-out::<fixnum> height-out::<fixnum>
;                                type-out::<fixnum> data-out::<uvector>)


;;=============================================================
;; Quadrics
;;

(inline-stub
 (declare-stub-type <glu-quadric> "ScmGluQuadric*" #f
   "SCM_GLU_QUADRIC_P" "SCM_GLU_QUADRIC"))

;; gluNewQuadric : (make <glu-quadric>)
;; gluDeleteQuadric : GC takes care of this

(define-cproc glu-quadric-draw-style (quad::<glu-quadric> style::<fixnum>)
  ::<void> (gluQuadricDrawStyle (-> quad quadric) style))

(define-cproc glu-quadric-orientation (quad::<glu-quadric> orientation::<fixnum>)
  ::<void> (gluQuadricOrientation (-> quad quadric) orientation))

(define-cproc glu-quadric-normals (quad::<glu-quadric> normals::<fixnum>)
  ::<void> (gluQuadricNormals (-> quad quadric) normals))

(define-cproc glu-quadric-texture (quad::<glu-quadric> texcoords::<fixnum>)
  ::<void> (gluQuadricTexture (-> quad quadric) texcoords))

;; glu-quadric-callback - gluQuadricCallback doesn't pass the closure
;;   information, so it is practially impossible to implement this in
;;   reasonable way.

(define-cproc glu-cylinder (quad::<glu-quadric> base-radius::<double>
                            top-radius::<double> height::<double>
                            slices::<fixnum> stacks::<fixnum>)
  ::<void>
  (gluCylinder (-> quad quadric) base_radius top_radius height slices stacks))

(define-cproc glu-sphere (quad::<glu-quadric> radius::<double>
                          slices::<fixnum> stacks::<fixnum>)
  ::<void>
  (gluSphere (-> quad quadric) radius slices stacks))

(define-cproc glu-disk (quad::<glu-quadric>
                        inner-radius::<double> outer-radius::<double>
                        slices::<fixnum> loops::<fixnum>)
  ::<void>
  (gluDisk (-> quad quadric) inner_radius outer_radius slices loops))

(define-cproc glu-partial-disk (quad::<glu-quadric>
                                inner-radius::<double> outer-radius::<double>
                                slices::<fixnum> loops::<fixnum>
                                start-angle::<double> sweep-angle::<double>)
  ::<void>
  (gluPartialDisk (-> quad quadric) inner_radius outer_radius slices loops
                  start_angle sweep_angle))


;;=============================================================
;; Nurbs
;;

(inline-stub
 (declare-stub-type <glu-nurbs> "ScmGluNurbs*" #f
   "SCM_GLU_NURBS_P" "SCM_GLU_NURBS"))

;; glu-new-nurbs-renderer : (make <glu-nurbs>)
;; glu-delete-nurbs-renderer : GC takes care of this.

(define-cproc glu-load-sampling-matrices (nurbs::<glu-nurbs>
                                          model-matrix proj-matrix viewport)
  ::<void>
  (assert-vector-type&size f32 16 model-matrix)
  (assert-vector-type&size f32 16 proj-matrix)
  (assert-vector-type&size s32 4 viewport)
  (gluLoadSamplingMatrices (-> nurbs nurbs)
                           (SCM_F32VECTOR_ELEMENTS model_matrix)
                           (SCM_F32VECTOR_ELEMENTS proj_matrix)
                           (cast GLint* (SCM_S32VECTOR_ELEMENTS viewport))))

(define-cproc glu-nurbs-property (nurbs::<glu-nurbs>
                                  property::<fixnum> value::<double>)
  ::<void> (gluNurbsProperty (-> nurbs nurbs) property value))

(define-cproc glu-get-nurbs-property (nurbs::<glu-nurbs> property::<fixnum>)
  ::<float>
  (let* ([value::GLfloat])
    (gluGetNurbsProperty (-> nurbs nurbs) property (& value))
    (return value)))

(define-cproc glu-begin-curve (nurbs::<glu-nurbs>) ::<void>
  (gluBeginCurve (-> nurbs nurbs)))
(define-cproc glu-end-curve (nurbs::<glu-nurbs>) ::<void>
  (gluEndCurve (-> nurbs nurbs)))

(define-cproc glu-nurbs-curve (nurbs::<glu-nurbs>
                               knot::<f32vector> stride::<fixnum>
                               ctlarray::<f32vector>
                               order::<fixnum> type::<fixnum>)
  ::<void>
  (let* ([nknots::int (SCM_F32VECTOR_SIZE knot)]
         [nctlarray::int (* stride (- nknots order))])
    (unless (== nctlarray (SCM_F32VECTOR_SIZE ctlarray))
      (Scm_Error "f32vector of length %d is required for control points, but got %S" nctlarray ctlarray))
    (gluNurbsCurve (-> nurbs nurbs) nknots (SCM_F32VECTOR_ELEMENTS knot)
                   stride (SCM_F32VECTOR_ELEMENTS ctlarray) order type)))

(define-cproc glu-begin-surface (nurbs::<glu-nurbs>) ::<void>
  (gluBeginSurface (-> nurbs nurbs)))
(define-cproc glu-end-surface (nurbs::<glu-nurbs>) ::<void>
  (gluEndSurface (-> nurbs nurbs)))

;;================================================================
;; Polygon tesselation
;;

(inline-stub
 (declare-stub-type <glu-tesselator> "ScmGluTesselator*" #f
   "SCM_GLU_TESSELATOR_P" "SCM_GLU_TESSELATOR"))

;;=============================================================
;; Mipmapping and image scaling
;;

;; caller must ensure vector has enough length
(define-cproc glu-build-1d-mipmaps (target::<fixnum>
                                    internalformat::<fixnum>
                                    width::<fixnum>
                                    format::<fixnum> type::<fixnum>
                                    texels)
  ::<int>
  (let* ([elttype::int]
         [size::int (Scm_GLPixelDataSize width 1 format type (& elttype) NULL)]
         [texelptr::void* (Scm_GLPixelDataCheck texels elttype size)])
    (return (gluBuild1DMipmaps target internalformat width format
                               type texelptr))))

(define-cproc glu-build-2d-mipmaps (target::<fixnum>
                                    internalformat::<fixnum>
                                    width::<fixnum> height::<fixnum>
                                    format::<fixnum>
                                    type::<fixnum> texels)
  ::<int>
  (let* ([elttype::int]
         [size::int (Scm_GLPixelDataSize width height format type
                                         (& elttype) NULL)]
         [texelptr::void* (Scm_GLPixelDataCheck texels elttype size)])
    (return (gluBuild2DMipmaps target internalformat width height format
                               type texelptr))))

;; NB: We exclude HAVE_GL_GLEW_H case since glew defines GL_VERSION_1_2,
;; although MinGW's glu header doesn't have entry for these APIs.
(inline-stub
(.when (and (defined GL_VERSION_1_2)
            (not (defined HAVE_GL_GLEW_H)))

  (define-cproc glu-build-3d-mipmaps (target::<fixnum>
                                      internalformat::<fixnum>
                                      width::<fixnum> height::<fixnum>
                                      depth::<fixnum> format::<fixnum>
                                      type::<fixnum> texels)
    ::<int>
    ;; NB: does Scm_GLPxielDataSize work to pass height*depth as height?
    ;; need to think over it.
    (let* ([elttype::int]
           [size::int (Scm_GLPixelDataSize width (* height depth) format type
                                           (& elttype) NULL)]
           [texelptr::void* (Scm_GLPixelDataCheck texels elttype size)])
      (return (gluBuild3DMipmaps target internalformat width height depth format
                                 type texelptr))))

  (define-cproc glu-build-1d-mipmap-levels (target::<fixnum>
                                            internalformat::<fixnum>
                                            width::<fixnum>
                                            format::<fixnum> type::<fixnum>
                                            level::<fixnum> base::<fixnum>
                                            max::<fixnum>
                                            texels)
    ::<int>
    (let* ([elttype::int]
           [size::int (Scm_GLPixelDataSize width 1 format type (& elttype) NULL)]
           [texelptr::void* (Scm_GLPixelDataCheck texels elttype size)])
      (return (gluBuild1DMipmapLevels target internalformat width format
                                      type level base max texelptr))))

  (define-cproc glu-build-2d-mipmap-levels (target::<fixnum>
                                            internalformat::<fixnum>
                                            width::<fixnum> height::<fixnum>
                                            format::<fixnum> type::<fixnum>
                                            level::<fixnum> base::<fixnum>
                                            max::<fixnum>
                                            texels)
    ::<int>
    (let* ([elttype::int]
           [size::int (Scm_GLPixelDataSize width height format type
                                           (& elttype) NULL)]
           [texelptr::void* (Scm_GLPixelDataCheck texels elttype size)])
      (return (gluBuild2DMipmapLevels target internalformat width height format
                                      type level base max texelptr))))

  (define-cproc glu-build-3d-mipmap-levels (target::<fixnum>
                                            internalformat::<fixnum>
                                            width::<fixnum> height::<fixnum>
                                            depth::<fixnum> format::<fixnum>
                                            type::<fixnum> level::<fixnum>
                                            base::<fixnum> max::<fixnum>
                                            texels)
    ::<int>
    ;; NB: does Scm_GLPxielDataSize work to pass height*depth as height?
    ;; need to think over it.
    (let* ([elttype::int]
           [size::int (Scm_GLPixelDataSize width (* height depth) format type
                                           (& elttype) NULL)]
           [texelptr::void* (Scm_GLPixelDataCheck texels elttype size)])
      (return (gluBuild3DMipmapLevels target internalformat width height depth
                                      format type level base max texelptr))))

  )
)

;;================================================================
;; New function in GLU 1.1
;;

(define-cproc glu-get-string (name::<fixnum>)
  (let* ([p::(const GLubyte*) (gluGetString name)])
    (if p
      (return (Scm_MakeString (cast (const char*) p) -1 -1 0))
      (return SCM_FALSE))))

;;================================================================
;; Constants
;;

;; Normal vectors
(define-enum GLU_SMOOTH)
(define-enum GLU_FLAT)
(define-enum GLU_NONE)

;; Quadric draw styles
(define-enum GLU_POINT)
(define-enum GLU_LINE)
(define-enum GLU_FILL)
(define-enum GLU_SILHOUETTE)

;; Quadric orientation
(define-enum GLU_OUTSIDE)
(define-enum GLU_INSIDE)

;; Tesselator
(inline-stub
 (.unless (defined __CYGWIN__)
   (define-enum GLU_BEGIN)
   (define-enum GLU_VERTEX)
   (define-enum GLU_END)
   (define-enum GLU_ERROR)
   (define-enum GLU_EDGE_FLAG)))

;; Contour types
(inline-stub
 (.unless (defined __CYGWIN__)
   (define-enum GLU_CW)
   (define-enum GLU_CCW)
   (define-enum GLU_INTERIOR)
   (define-enum GLU_EXTERIOR)
   (define-enum GLU_UNKNOWN)))

;; Tesselation errors
(define-enum GLU_TESS_ERROR1)
(define-enum GLU_TESS_ERROR2)
(define-enum GLU_TESS_ERROR3)
(define-enum GLU_TESS_ERROR4)
(define-enum GLU_TESS_ERROR5)
(define-enum GLU_TESS_ERROR6)
(define-enum GLU_TESS_ERROR7)
(define-enum GLU_TESS_ERROR8)
;(define-enum GLU_TESS_ERROR9)

;; NURBS
(define-enum GLU_AUTO_LOAD_MATRIX)
(define-enum GLU_CULLING)
(define-enum GLU_PARAMETRIC_TOLERANCE)
(define-enum GLU_SAMPLING_TOLERANCE)
(define-enum GLU_DISPLAY_MODE)
(define-enum GLU_SAMPLING_METHOD)
(define-enum GLU_U_STEP)
(define-enum GLU_V_STEP)

(define-enum GLU_PATH_LENGTH)
(define-enum GLU_PARAMETRIC_ERROR)
(define-enum GLU_DOMAIN_DISTANCE)

(define-enum GLU_MAP1_TRIM_2)
(define-enum GLU_MAP1_TRIM_3)

(define-enum GLU_OUTLINE_POLYGON)
(define-enum GLU_OUTLINE_PATCH)

(define-enum GLU_NURBS_ERROR1)
(define-enum GLU_NURBS_ERROR2)
(define-enum GLU_NURBS_ERROR3)
(define-enum GLU_NURBS_ERROR4)
(define-enum GLU_NURBS_ERROR5)
(define-enum GLU_NURBS_ERROR6)
(define-enum GLU_NURBS_ERROR7)
(define-enum GLU_NURBS_ERROR8)
(define-enum GLU_NURBS_ERROR9)
(define-enum GLU_NURBS_ERROR10)
(define-enum GLU_NURBS_ERROR11)
(define-enum GLU_NURBS_ERROR12)
(define-enum GLU_NURBS_ERROR13)
(define-enum GLU_NURBS_ERROR14)
(define-enum GLU_NURBS_ERROR15)
(define-enum GLU_NURBS_ERROR16)
(define-enum GLU_NURBS_ERROR17)
(define-enum GLU_NURBS_ERROR18)
(define-enum GLU_NURBS_ERROR19)
(define-enum GLU_NURBS_ERROR20)
(define-enum GLU_NURBS_ERROR21)
(define-enum GLU_NURBS_ERROR22)
(define-enum GLU_NURBS_ERROR23)
(define-enum GLU_NURBS_ERROR24)
(define-enum GLU_NURBS_ERROR25)
(define-enum GLU_NURBS_ERROR26)
(define-enum GLU_NURBS_ERROR27)
(define-enum GLU_NURBS_ERROR28)
(define-enum GLU_NURBS_ERROR29)
(define-enum GLU_NURBS_ERROR30)
(define-enum GLU_NURBS_ERROR31)
(define-enum GLU_NURBS_ERROR32)
(define-enum GLU_NURBS_ERROR33)
(define-enum GLU_NURBS_ERROR34)
(define-enum GLU_NURBS_ERROR35)
(define-enum GLU_NURBS_ERROR36)
(define-enum GLU_NURBS_ERROR37)

;; Errors
(define-enum GLU_INVALID_ENUM)
(define-enum GLU_INVALID_VALUE)
(define-enum GLU_OUT_OF_MEMORY)

(inline-stub
 (.when (defined GLU_INCOMPATIBLE_GL_VERSION)
   (define-enum GLU_INCOMPATIBLE_GL_VERSION)))

;; New in GLU 1.1
(define-enum GLU_VERSION)
(define-enum GLU_EXTENSIONS)

;; Local variables:
;; mode: scheme
;; end:
