;; Example 13-2  Selection Example

(use gl)
(use gl.glut)
(use gauche.uvector)

(define (draw-triangle xs ys z)
  (gl-begin* GL_TRIANGLES
    (for-each (cut gl-vertex <> <> z) xs ys)))

(define (draw-view-volume x1 x2 y1 y2 z1 z2)
  (define (line-loop z)
    (gl-vertex x1 y1 z)
    (gl-vertex x2 y1 z)
    (gl-vertex x2 y2 z)
    (gl-vertex x1 y2 z))
  (define (line x y)
    (gl-vertex x y (- z1))
    (gl-vertex x y (- z2)))
  
  (gl-color 1.0 1.0 1.0)
  (gl-begin* GL_LINE_LOOP (line-loop (- z1)))
  (gl-begin* GL_LINE_LOOP (line-loop (- z2)))
  (gl-begin* GL_LINES (line x1 y1) (line x1 y2) (line x2 y1) (line x2 y2))
  )

(define (draw-scene)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-perspective 40.0 4/3 1.0 100.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  (glu-look-at 7.5 7.5 12.5 2.5 2.5 -5.0 0.0 1.0 0.0)
  (gl-color 0.0 1.0 0.0)
  (draw-triangle '(2.0 3.0 2.5) '(2.0 2.0 3.0) -5.0)
  (gl-color 1.0 0.0 0.0)
  (draw-triangle '(2.0 3.0 2.5) '(7.0 7.0 8.0) -5.0)
  (gl-color 1.0 1.0 0.0)
  (draw-triangle '(2.0 3.0 2.5) '(2.0 2.0 3.0) -1.0)
  (draw-triangle '(2.0 3.0 2.5) '(2.0 2.0 3.0) -9.0)
  (draw-view-volume 0.0 5.0 0.0 5.0 0.0 10.0))

(define (process-hits hits vec)
  (print #`"hits = ,hits")
  (let1 ptr 0
    (dotimes (i hits)
      (let1 names (ref vec ptr)
        (print #`" number of names for hit = ,names")
        (inc! ptr)
        (print #`"  z1 is ,(/ (ref vec ptr) #x7fffffff);"
               #`" z2 is ,(/ (ref vec (+ ptr 1)) #x7fffffff)")
        (inc! ptr 2)
        (display "   the name is ")
        (dotimes (j names)
          (display #`",(ref vec ptr) ") (inc! ptr))
        (print)))))

(define-constant BUFSIZE 512)

(define (select-objects)
  (let1 select-buf (make-u32vector BUFSIZE)
    (gl-select-buffer select-buf)
    (gl-render-mode GL_SELECT)

    (gl-init-names)
    (gl-push-name 0)

    (gl-push-matrix)
    (gl-matrix-mode GL_PROJECTION)
    (gl-load-identity)
    (gl-ortho 0.0 5.0 0.0 5.0 0.0 10.0)
    (gl-matrix-mode GL_MODELVIEW)
    (gl-load-identity)
    (gl-load-name 1)
    (draw-triangle '(2.0 3.0 2.5) '(2.0 2.0 3.0) -5.0)
    (gl-load-name 2)
    (draw-triangle '(2.0 3.0 2.5) '(7.0 7.0 8.0) -5.0)
    (gl-load-name 3)
    (draw-triangle '(2.0 3.0 2.5) '(2.0 2.0 3.0) -1.0)
    (draw-triangle '(2.0 3.0 2.5) '(2.0 2.0 3.0) -9.0)
    (gl-pop-matrix)
    (gl-flush)

    (process-hits (gl-render-mode GL_RENDER) select-buf)
    ))

(define (init)
  (gl-enable GL_DEPTH_TEST)
  (gl-shade-model GL_FLAT))

(define (disp)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (draw-scene)
  (select-objects)
  (gl-flush))

(define (keyboard key x y)
  (when (= key 27) (exit 0)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size 200 200)
  (glut-init-window-position 100 100)
  (glut-create-window (car args))
  (init)
  (glut-display-func disp)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)
