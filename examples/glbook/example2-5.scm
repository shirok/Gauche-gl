;; Example 2-5  Line Stipple Patterns

(use gl)
(use gl.glut)

(define-syntax draw-one-line
  (syntax-rules ()
    ((_ x1 y1 x2 y2)
     (begin (gl-begin GL_LINES)
            (gl-vertex x1 y1)
            (gl-vertex x2 y2)
            (gl-end)))))

(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)
  )

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color '#f32(1.0 1.0 1.0))
  (gl-enable GL_LINE_STIPPLE)

  (gl-line-stipple 1 #x0101)            ;dotted
  (draw-one-line 50.0 125.0 150.0 125.0)
  (gl-line-stipple 1 #x00ff)            ;dashed
  (draw-one-line 150.0 125.0 250.0 125.0)
  (gl-line-stipple 1 #x1c47)            ;dash/dot/dash
  (draw-one-line 250.0 125.0 350.0 125.0)
  
  (gl-line-width 5.0)
  (gl-line-stipple 1 #x0101)            ;dotted
  (draw-one-line 50.0 100.0 150.0 100.0)
  (gl-line-stipple 1 #x00ff)            ;dashed
  (draw-one-line 150.0 100.0 250.0 100.0)
  (gl-line-stipple 1 #x1c47)            ;dash/dot/dash
  (draw-one-line 250.0 100.0 350.0 100.0)
  (gl-line-width 1.0)

  (gl-line-stipple 1 #x1c47)            ;dash/dot/dash
  (gl-begin GL_LINE_STRIP)
  (do ((i 0 (+ i 1)))
      ((= i 7))
    (gl-vertex (+ 50.0 (* i 50.0)) 75.0))
  (gl-end)

  (do ((i 0 (+ i 1)))
      ((= i 6))
    (draw-one-line (+ 50.0 (* i 50.0)) 50.0
                   (+ 50.0 (* (+ i 1) 50.0)) 50.0))

  (gl-line-stipple 5 #x1c47)
  (draw-one-line 50.0 25.0 350.0 25.0)

  (gl-disable GL_LINE_STIPPLE)
  (gl-flush)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d 0 w 0 h)
  )

(define (keyboard key x y)
  (when (= key 27) (exit 0)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 400 150)
  (glut-init-window-position 100 100)
  (glut-create-window *program-name*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)

  
    

  