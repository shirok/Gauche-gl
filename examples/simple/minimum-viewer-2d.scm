#!/bin/sh
:; exec gosh -I../../src -I../../lib $0 "$@"
;; A minimum demo to use gl.simple.viewer

(use gl)
(use gl.glut)
(use gl.simple.viewer)
(use math.const)

(define (main args)
  (glut-init args)
  (simple-viewer-display
      (^[state]
        (gl-begin* GL_LINE_STRIP
          (dotimes [i 1000]
            (let1 x (* (- i 500) 0.01)
              (gl-vertex x (sin (* pi x))))))
        (gl-begin* GL_LINE_LOOP
          (dotimes [i 100]
            (let1 t (* i 0.01)
              (gl-vertex (cos (* 2 pi t)) (sin (* 2 pi t))))))))
  (simple-viewer-window-2d 'demo :zoom 200)
  (simple-viewer-run)
  0)

;; Local variables:
;; mode: scheme
;; end:
