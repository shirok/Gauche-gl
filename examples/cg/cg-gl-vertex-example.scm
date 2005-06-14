; This is a port, to Gauche, of a demo included with the Cg distribution.
; Port by Issac Trotts, 2005

(use gl)
(use gl.glut)
(use gl.math3d)
(use gl.cg)
(use gauche.array)
(use gauche.uvector)
(use gauche.sequence)
(use srfi-1)  ; list ops

;; Static Data

(define *texture-res* 512)

(define *context* #f)
(define *program* #f)
(define *kd-param* #f)  ; diffuse lighting coefficient
(define *model-view-proj-param* #f)
(define *test-color-param* #f)

(define *profile* #f)

(define *light-diffuse* (f32vector 1 0 0 1))
(define *light-position* (f32vector 1 1 1 0))

(define *cube-normals*
  (list
    (vector4f -1.0 0.0 0.0)
    (vector4f 0.0 1.0 0.0)
    (vector4f 1.0 0.0 0.0)
    (vector4f 0.0 -1.0 0.0)
    (vector4f 0.0 0.0 1.0)
    (vector4f 0.0 0.0 -1.0)
    ))

(define *cube-faces*
  #,(<u32array> (0 6  0 4)
      0 1 2 3
      3 2 6 7
      7 6 5 4
      4 5 1 0
      5 6 2 1
      7 4 0 3))

(define *cube-vertices* 
  (let* ((v (list-tabulate 8 (lambda (i) (vector4f 0 0 0))))
         (multiset (lambda (indices val) 
                     (for-each (lambda (ij) (set! (ref (list-ref v (car ij)) 
                                                       (cadr ij)) 
                                              val))
                               indices))))
    (multiset '((0 0) (1 0) (2 0) (3 0)) -1)
    (multiset '((4 0) (5 0) (6 0) (7 0)) 1)
    (multiset '((0 1) (1 1) (4 1) (5 1)) -1)
    (multiset '((2 1) (3 1) (6 1) (7 1)) 1)
    (multiset '((0 2) (3 2) (4 2) (7 2)) 1)
    (multiset '((1 2) (2 2) (5 2) (6 2)) -1)
    v
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-cube)

  ;
  ;; Set various uniform parameters including the Model-view-projection
  ;; matrix for transforming the incoming position into HPOS.
  ;
  (if *kd-param*
    (cg-gl-set-parameter *kd-param* 1.0 1.0 0.0 1.0))
    
  ;; Set the concatenated modelview and projection matrices 
  (if *model-view-proj-param*
    (cg-gl-set-state-matrix-parameter *model-view-proj-param* 
                                      CG_GL_MODELVIEW_PROJECTION_MATRIX
                                      CG_GL_MATRIX_IDENTITY))

  (cg-gl-bind-program *program*)
  (cg-gl-enable-profile *profile*)

  ;
  ;; Create cube with per-vertex varying attributes 
  ;
  (dotimes (i 6)
    (gl-begin GL_QUADS)

    (gl-normal (list-ref *cube-normals* 0))
    (cg-gl-set-parameter *test-color-param* 1.0 0.0 0.0)
    (gl-vertex (list-ref *cube-vertices* (array-ref *cube-faces* i 0)))

    (cg-gl-set-parameter *test-color-param* 0.0 1.0 0.0)
    (gl-vertex (list-ref *cube-vertices* (array-ref *cube-faces* i 1)))

    (cg-gl-set-parameter *test-color-param* 0.0 0.0 1.0)
    (gl-vertex (list-ref *cube-vertices* (array-ref *cube-faces* i 2)))

    (cg-gl-set-parameter *test-color-param* 1.0 1.0 1.0)
    (gl-vertex (list-ref *cube-vertices* (array-ref *cube-faces* i 3)))
    
    (gl-end)
    )

  (cg-gl-disable-profile *profile*)
  )

(define (display)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (draw-cube)
  (glut-swap-buffers)
  )

(define (initialize-glut args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-create-window *program-name*)
  (glut-display-func display)

  ;; Enable a single Open-gL light. 
  (gl-light GL_LIGHT0  GL_DIFFUSE  *light-diffuse*)
  (gl-light GL_LIGHT0  GL_POSITION  *light-position*)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_LIGHTING)

  ;; Use depth buffering for hidden surface elimination. 
  (gl-enable GL_DEPTH_TEST)

  ;; Setup the view of the cube. 
  (gl-matrix-mode GL_PROJECTION)
  (glu-perspective 40.0 ; field of view in degree 
    1.0 ; aspect ratio 
    1.0 ; Z near
    10.0 ; zfar
    )
  (gl-matrix-mode GL_MODELVIEW)
  (glu-look-at 0.0 0.0 5.0 ; eye is at (0 0 5) 
               0.0 0.0 0.0 ; center is at (0 0 0) 
               0.0 1.0 0.0) ; up is in positive Y direction 

  ;; Adjust cube position to be asthetic angle. 
  (gl-translate 0.0 0.0 -1.0)
  (gl-rotate 60 1.0 0.0 0.0)
  (gl-rotate -20 0.0 0.0 1.0)
  )

(define (main args)
  (initialize-glut args)

  (set! *profile*
    (cond
      ((cg-gl-is-profile-supported CG_PROFILE_VP20)   CG_PROFILE_VP20)
      ((cg-gl-is-profile-supported CG_PROFILE_ARBVP1) CG_PROFILE_ARBVP1)
      (else
        (format #t "Video card does not support vertex programs exiting...\n")
        (exit -1))))

  ;; Test cg-context creation 
  (set! *context* (cg-create-context))

  ;; Test adding source text to context 
  (set! *program*
    (cg-create-program-from-file *context* CG_SOURCE "cg-gl-vertex-example.cg" 
                                 *profile* "main" #f))

  (print "---- PROGRAM BEGIN ----")
  (print (cg-get-program-string *program* CG_COMPILED_PROGRAM))
  (print "---- PROGRAM END ----")
  (flush)

  (if *program*
    (begin
      (cg-gl-load-program *program*)

      (set! *kd-param* (cg-get-named-parameter *program* "Kd"))

      (set! *model-view-proj-param*
        (cg-get-named-parameter *program* "ModelViewProj"))

      (set! *test-color-param* (cg-get-named-parameter *program* "IN.TestColor"))
      )
    (begin
      (format #t "Invalid GPU program\n") 
      (flush)
      (exit 1)))

  (glut-main-loop)

  ; (cg-destroy-program *program*)
  ; (cg-destroy-context *context*)

  0)

