;; Example 6-3  Antialised Lines

(use gl)
(use gl.glut)

(define *rot-angle* 0)

(define (init)
  (let ((g (gl-get-float GL_LINE_WIDTH_GRANULARITY)))
    (format #t "GL_LINE_WIDTH_GRANULARITY value is ~s\n" g))
  (let ((r (gl-get-float GL_LINE_WIDTH_RANGE)))
    (format #t "GL_LINE_WIDTH_RANGE values are ~s ~s\n" (ref r 0) (ref r 1)))

  (gl-enable GL_LINE_SMOOTH)
  (gl-enable GL_BLEND)
  (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
  (gl-hint GL_LINE_SMOOTH_HINT GL_DONT_CARE)
  (gl-line-width 1.5)

  (gl-clear-color 0.0 0.0 0.0 0.0)
  )

(define (display)
  (gl-clear GL_COLOR_BUFFER_BIT)

  (gl-color 0.0 1.0 0.0)
  (gl-push-matrix)
  (gl-rotate (- *rot-angle*) 0.0 0.0 0.1)
  (gl-begin GL_LINES)
  (gl-vertex -0.5 0.5)
  (gl-vertex 0.5 -0.5)
  (gl-end)
  (gl-pop-matrix)

  (gl-color 0.0 0.0 1.0)
  (gl-push-matrix)
  (gl-rotate *rot-angle* 0.0 0.0 0.1)
  (gl-begin GL_LINES)
  (gl-vertex 0.5 0.5)
  (gl-vertex -0.5 -0.5)
  (gl-end)
  (gl-pop-matrix)

  (gl-flush)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (if (<= w h)
      (glu-ortho-2d -1.0 1.0 (* -1.0 (/ h w)) (* 1.0 (/ h w)))
      (glu-ortho-2d (* -1.0 (/ w h)) (* 1.0 (/ w h)) -1.0 1.0))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  )

(define (keyboard key x y)
  (cond
   ((or (= key (char->integer #\r))
        (= key (char->integer #\R)))
    (inc! *rot-angle* 20.0)
    (if (>= *rot-angle* 360.0) (set! *rot-angle* 0.0))
    (glut-post-redisplay))
   ((= key 27)                          ;ESC
    (exit 0)))
  )

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 200 200)
  (glut-create-window (car args))
  (init)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-display-func display)
  (glut-main-loop)
  0)
