#!/bin/sh
:; exec gosh -I../../src -I../../lib $0 "$@"

;; Simple animation demo

(use gl)
(use gl.glut)
(use gl.simple.viewer)
(use gl.simple.scene-time)
(use math.const)

;; Drawing shape at the current scnee time.
;; This sample draws simple spiral motion.  You can change trajectory procedure
;; to try out diffent motions.
(define *vertical-speed* 0.3)   ; unit/sec
(define *horizontal-radius* 3.0)
(define *horizontal-speed* 2pi)  ; read/sec

(define (trajectory)
  (define T (current-scene-time))
  (define dt (/. 32))
  (gl-begin* GL_LINE_STRIP
    (let loop ((i 0))
      (let1 t (* i dt)
        (when (< t T)
          (let ([x (* *horizontal-radius* (cos (* *horizontal-speed* t)))]
                [z (* *horizontal-radius* (sin (* *horizontal-speed* t)))]
                [y (* *vertical-speed* t)])
            (gl-vertex x y z))
          (loop (+ i 1)))))))


(define (usage)
  (print "Key bindings:")
  (print "  R   : Reset scene time")
  (print "  SPC : Toggle run/step mode")
  (print "  >   : Step one unit forward")
  (print "  <   : Step one unit back")
  (print "  +   : Speed up scene progress")
  (print "  -   : Slow down scene progress")
  (print "  ESC : Exit"))

(define (main args)
  (glut-init args)
  (simple-viewer-set-key! #f
                          #\r (^ _ (reset-scene-time!))
                          #\space (^ _ (toggle-scene-time-mode!))
                          #\< (^ _ (step-scene-time! -1))
                          #\> (^ _ (step-scene-time! 1))
                          #\+ (^ _ (update! (scene-time-speed-factor) (pa$ * 2.0)))
                          #\- (^ _ (update! (scene-time-speed-factor)
                                            (^x (max 0.125 (/ x 2.0)))))
                          #\? (^ _ (usage)))
  (set! (scene-time-stepper) 0.15)
  (simple-viewer-display (^[state]
                           (trajectory)
                           (glut-post-redisplay)))
  (simple-viewer-window 'demo)
  (simple-viewer-run)
  0)

;; Local variables:
;; mode: scheme
;; end:
