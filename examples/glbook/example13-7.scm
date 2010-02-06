;; Example 13-7  Feedback Mode

(use gl)
(use gl.glut)
(use gauche.uvector)
(use srfi-1)

(define (init)
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0))

(define (draw-geometry mode)
  (gl-begin* GL_LINE_STRIP
    (gl-normal 0.0 0.0 1.0)
    (gl-vertex 30.0 30.0 0.0)
    (gl-vertex 50.0 60.0 0.0)
    (gl-vertex 70.0 40.0 0.0)
    )
  (when (= mode GL_FEEDBACK) (gl-pass-through 1.0))
  (gl-begin* GL_POINTS
    (gl-vertex -100.0 -100.0 -100.0)  ;;  will be clipped
    )
  (when (= mode GL_FEEDBACK) (gl-pass-through 2.0))
  (gl-begin* GL_POINTS
    (gl-normal 0.0 0.0 1.0)
    (gl-vertex 50.0 50.0 0.0)
    )
  )

(define (print-3d-color-vertex buffer count)
  (display "  ")
  (for-each (lambda (i) (display #`",(ref buffer (+ count i)) "))
            (iota 8))
  (print))

(define (print-buffer size buffer)
  (do ((count 0 count))
      ((>= count size))
    (let1 token (begin0 (ref buffer count) (inc! count))
      (cond
       ((= token GL_PASS_THROUGH_TOKEN)
        (print "GL_PASS_THOURGH_TOKEN")
        (print #`"  ,(ref buffer count)") (inc! count))
       ((= token GL_POINT_TOKEN)
        (print "GL_POINT_TOKEN")
        (print-3d-color-vertex buffer count) (inc! count 8))
       ((= token GL_LINE_TOKEN)
        (print "GL_LINE_TOKEN")
        (print-3d-color-vertex buffer count) (inc! count 8)
        (print-3d-color-vertex buffer count) (inc! count 8))
       ((= token GL_LINE_RESET_TOKEN)
        (print "GL_LINE_RESET_TOKEN")
        (print-3d-color-vertex buffer count) (inc! count 8)
        (print-3d-color-vertex buffer count) (inc! count 8))
       ))))

(define *buffer* (make-f32vector 1024 0))

(define (disp)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho 0.0 100.0 0.0 100.0 0.0 1.0)

  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (draw-geometry GL_RENDER)

  (gl-feedback-buffer GL_3D_COLOR *buffer*)
  (gl-render-mode GL_FEEDBACK)
  (draw-geometry GL_FEEDBACK)

  (print-buffer (gl-render-mode GL_RENDER) *buffer*))

(define (keyboard key x y)
  (when (= key 27) (exit 0)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 100 100)
  (glut-init-window-position 100 100)
  (glut-create-window (car args))
  (init)
  (glut-display-func disp)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)

