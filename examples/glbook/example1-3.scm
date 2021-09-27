;; Example 1-3

(use gl)
(use gl.glut)

(define *spin* 0.0)

(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)
  )

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-push-matrix)
  (gl-rotate *spin* 0.0 0.0 1.0)
  (gl-color '#f32(1.0 1.0 1.0))
  (gl-rect '#f32(-25.0 -25.0) '#f32(25.0 25.0))
  (gl-pop-matrix)
  (glut-swap-buffers)
  )

(define (spin-display)
  (set! *spin* (modulo (+ *spin* 2.0) 360.0))
  (glut-post-redisplay)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho -50.0 50.0 -50.0 50.0 -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  )

(define (mouse button state x y)
  (cond
    ((= button GLUT_LEFT_BUTTON)
     (when (= state GLUT_DOWN) (glut-idle-func spin-display)))
    ((= button GLUT_MIDDLE_BUTTON)
     (when (= state GLUT_DOWN) (glut-idle-func #f))))
  )

(define (keyboard key x y)
  (cond
   ((= key 27) (exit 0))
   ))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB))
  (glut-init-window-size 250 250)
  (glut-init-window-position 100 100)
  (glut-create-window *program-name*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-mouse-func mouse)
  (glut-main-loop)
  0)
