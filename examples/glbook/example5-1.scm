;; Example 5-1 Drawing a Lit Sphere

(use gl)
(use gl.glut)

(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_SMOOTH)
  (gl-material GL_FRONT GL_SPECULAR '#f32(1.0 1.0 1.0 1.0))
  (gl-material GL_FRONT GL_SHININESS 50.0)
  (gl-light GL_LIGHT0 GL_POSITION '#f32(1.0 1.0 1.0 0.0))
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_DEPTH_TEST)
  )

(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (glut-solid-sphere 1.0 20 16)
  (gl-flush)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (if (<= w h)
      (gl-ortho -1.5 1.5 (* -1.5 (/ h w)) (* 1.5 (/ h w)) -10.0 10.0)
      (gl-ortho (* -1.5 (/ w h)) (* 1.5 (/ w h)) -1.5 1.5 -10.0 10.0))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  )


(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH)))
  (glut-init-window-size 500 500)
  (glut-init-window-position 100 100)
  (glut-create-window *program-name*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-main-loop)
  0)
