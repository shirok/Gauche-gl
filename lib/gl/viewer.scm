(use gl)
(use gl.glut)
(use gl.grid)

(define grid (ggl-make-grid 5 5))

(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model |GL_FLAT|)
  (gl-enable-client-state |GL_VERTEX_ARRAY|)
  )

(define (disp)
  (gl-clear |GL_COLOR_BUFFER_BIT|)
  (gl-color 1.0 1.0 1.0)
  (glut-solid-cube 1.0)
  (grid)
  (gl-translate 0 0 -10)
  (glut-solid-cube 1.0)
  (gl-translate 0 0 -20)
  (glut-solid-cube 1.0)
  (gl-translate 0 0 10)
  (glut-solid-cube 1.0)
  (gl-translate 0 0 20)
  (glut-solid-cube 1.0)
  (gl-flush)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode |GL_PROJECTION|)
  (gl-load-identity)
  (glu-perspective 90 (/ w h) 1.0 10000.0)
  (gl-matrix-mode |GL_MODELVIEW|)
  (gl-load-identity)
  (glu-look-at 0 0 10 0 0 0 0 1 0)
  )


(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior |GLUT_SINGLE| |GLUT_RGB|))
  (glut-init-window-size 500 500)
  (glut-init-window-position 100 100)
  (glut-create-window *program-name*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-main-loop)
  0)