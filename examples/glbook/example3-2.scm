;; Example 3-2  Using Modeling transformations

(use gl)
(use gl.glut)

(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)
  )

(define (draw-triangle)
  (gl-begin GL_LINE_LOOP)
  (gl-vertex '#f32(-20.0 -20.0))
  (gl-vertex '#f32(20.0 -20.0))
  (gl-vertex '#f32(0.0 14.6410161513775))
  (gl-end))

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  
  (gl-load-identity)
  (gl-color '#f32(1.0 1.0 1.0))
  (draw-triangle)

  (gl-enable GL_LINE_STIPPLE)
  (gl-line-stipple 1 #xf0f0)
  (gl-load-identity)
  (gl-translate -20.0 0.0 0.0)
  (draw-triangle)

  (gl-line-stipple 1 #xf00f)
  (gl-load-identity)
  (gl-scale 1.5 0.5 1.0)
  (draw-triangle)

  (gl-line-stipple 1 #x8888)
  (gl-load-identity)
  (gl-rotate 90.0 0.0 0.0 1.0)
  (draw-triangle)
  (gl-disable GL_LINE_STIPPLE)

  (gl-flush)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-frustum -20.0 20.0 -20.0 20.0 1.5 20.0)
  (glu-look-at 0.0 0.0 5.0 0.0 0.0 0.0 0.0 1.0 0.0)
  (gl-matrix-mode GL_MODELVIEW)
  )

(define (keyboard key x y)
  (when (= key 27) (exit 0)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 500 500)
  (glut-init-window-position 100 100)
  (glut-create-window *program-name*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)
