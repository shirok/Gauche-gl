#!/bin/sh
:; exec gosh -I../../src -I../../lib $0 "$@"
;; A minimum demo to use gl.simple.viewer

(use gl)
(use gl.glut)
(use gl.simple.viewer)

(define (main args)
  (glut-init args)
  (simple-viewer-display (lambda () (glut-wire-sphere 2.0 10 8)))
  (simple-viewer-window 'demo)
  (simple-viewer-run)
  0)
