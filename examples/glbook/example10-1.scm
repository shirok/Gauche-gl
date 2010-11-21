;; Example 10-1  Using the stencil test

(use gl)
(use gl.glut)

(define-constant YELLOWMAT 1)
(define-constant BLUEMAT   2)

(define (init)
  (let ((yellow-diffuse  '#f32(0.7 0.7 0.0 1.0))
        (yellow-specular '#f32(1.0 1.0 1.0 1.0))
        (blue-diffuse    '#f32(0.1 0.1 0.7 1.0))
        (blue-specular   '#f32(0.1 1.0 1.0 1.0))
        (position-one    '#f32(1.0 1.0 1.0 0.0))
        )
    (gl-new-list YELLOWMAT GL_COMPILE)
    (gl-material GL_FRONT GL_DIFFUSE yellow-diffuse)
    (gl-material GL_FRONT GL_SPECULAR yellow-specular)
    (gl-material GL_FRONT GL_SHININESS 45.0)
    (gl-end-list)

    (gl-new-list BLUEMAT GL_COMPILE)
    (gl-material GL_FRONT GL_DIFFUSE blue-diffuse)
    (gl-material GL_FRONT GL_SPECULAR blue-specular)
    (gl-material GL_FRONT GL_SHININESS 45.0)
    (gl-end-list)
    
    (gl-light GL_LIGHT0 GL_POSITION position-one)

    (gl-enable GL_LIGHT0)
    (gl-enable GL_LIGHTING)
    (gl-enable GL_DEPTH_TEST)

    ;(gl-clear-stencil 0)
    ;(gl-enable GL_STENCIL_TEST)
    ))

(define (display-proc)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  ;; draw blue sphere where the stencil is 1
  (gl-stencil-func GL_EQUAL 1 1)
  (gl-stencil-op GL_KEEP GL_KEEP GL_KEEP)
  (gl-call-list BLUEMAT)
  (glut-solid-sphere 0.5 15 15)

  ;; draw the tori where the tencil is not 1
  (gl-stencil-func GL_NOTEQUAL 1 1)
  (gl-push-matrix)
  (gl-rotate 45.0 0.0 0.0 1.0)
  (gl-rotate 45.0 0.0 1.0 0.0)
  (gl-call-list YELLOWMAT)
  (glut-solid-torus 0.275 0.85 30 30)
  (gl-push-matrix)
  (gl-rotate 90.0 1.0 0.0 0.0)
  (glut-solid-torus 0.275 0.85 30 30)
  (gl-pop-matrix)
  (gl-pop-matrix)
  (gl-flush)
  )

(define (reshape-proc w h)
  (gl-viewport 0 0 w h)
  ;; create a diamond shaped stencil area
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (if (<= w h)
    (glu-ortho-2d -3.0 3.0 (* -3.0 (/ h w)) (* 3.0 (/ h w)))
    (glu-ortho-2d (* -3.0 (/ w h)) (* 3.0 (/ w h)) -3.0 3.0))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)

  (gl-clear GL_STENCIL_BUFFER_BIT)
  (gl-stencil-func GL_ALWAYS 1 1)
  (gl-stencil-op GL_REPLACE GL_REPLACE GL_REPLACE)
  (gl-begin* GL_QUADS
    (gl-vertex -1.0 0.0)
    (gl-vertex 0.0 1.0)
    (gl-vertex 1.0 0.0)
    (gl-vertex 0.0 -1.0))

  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-perspective 45.0 (/ w h) 3.0 7.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  (gl-translate 0.0 0.0 -5.0)
  )

(define (keyboard-proc key x y)
  (when (= key 27) (exit 0)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH GLUT_STENCIL))
  (glut-init-window-size 400 400)
  (glut-init-window-position 100 100)
  (glut-create-window (car args))
  (init)
  (glut-reshape-func reshape-proc)
  (glut-display-func display-proc)
  (glut-keyboard-func keyboard-proc)
  (glut-main-loop)
  0)

                      

