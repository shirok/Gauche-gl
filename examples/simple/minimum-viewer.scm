;; A minimum demo to use gl.simple.viewer

(use gl)
(use gl.glut)
(use gl.simple.viewer)

(define (main args)
  (glut-init args)
  (simple-viewer-display (lambda () (glut-wire-sphere 2.0 10 8)))
  (simple-viewer-window :title "viewer-demo")
  (simple-viewer-run)
  0)
