;; Example 6-4  Antialiasing in Color-Index mode

(use gl)
(use gl.glut)

(define-constant RAMPSIZE 16)
(define-constant RAMP1START 32)
(define-constant RAMP2START 48)

(define *rot-angle* 0)

(define (init)
  ;; set up color palette
  (do ((i 0 (+ i 1)))
      ((= i RAMPSIZE))
    (let ((shade (/ i RAMPSIZE)))
      (glut-set-color (+ RAMP1START i) 0.0 shade 0.0)
      (glut-set-color (+ RAMP2START i) 0.0 0.0 shade)))

  (gl-enable GL_LINE_SMOOTH)
  (gl-hint GL_LINE_SMOOTH_HINT GL_DONT_CARE)
  (gl-line-width 1.5)

  (gl-clear-index RAMP1START)
  )

(define (display)
  (gl-clear GL_COLOR_BUFFER_BIT)

  (gl-index RAMP1START)
  (gl-push-matrix)
  (gl-rotate (- *rot-angle*) 0.0 0.0 0.1)
  (gl-begin GL_LINES)
  (gl-vertex -0.5 0.5)
  (gl-vertex 0.5 -0.5)
  (gl-end)
  (gl-pop-matrix)

  (gl-index RAMP2START)
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
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_INDEX))
  (glut-init-window-size 200 200)
  (glut-create-window (car args))
  (init)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-display-func display)
  (glut-main-loop)
  0)
