;; Example 1-2

(use gl)
(use gl.glut)

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color '#f32(1.0 1.0 1.0))
  (gl-begin* GL_POLYGON
    (gl-vertex '#f32(0.25 0.25 0.0))
    (gl-vertex '#f32(0.75 0.25 0.0))
    (gl-vertex '#f32(0.75 0.75 0.0))
    (gl-vertex '#f32(0.25 0.75 0.0))
    )
  (gl-flush)
  )

(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho 0.0 1.0 0.0 1.0 -1.0 1.0)
  )

(define (keyboard key x y)
  (cond
   ((= key 27) (exit 0))
   ))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 250 250)
  (glut-init-window-position 100 100)
  (glut-create-window "hello")
  (init)
  (glut-display-func disp)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0
  )

