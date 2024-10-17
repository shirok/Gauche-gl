#!/bin/sh
:; exec gosh -I../../src -I../../lib $0 "$@"

;; Simple animation demo

(use gl)
(use gl.glut)
(use gl.simple.viewer)
(use math.const)
(use scheme.time)

;; Represents absolute time in the scene
;;  In 'run' mode, current-scene-time consider the real time passed.
;;  in 'step' mode, current-scene-time is controlled by program.
(define-class <scene-time> ()
  ((mode :init-keyword :mode :init-value :run)
   (start-time :init-form (current-jiffy)) ;jiffies
   (current-time :init-value 0)))       ;seconds

(define-constant *stepper* 0.22)         ;seconds

(define (reset-scene-time! T)
  (set! (~ T'start-time) (current-jiffy))
  (set! (~ T'current-time) 0))

(define (current-scene-time T)
  (case (~ T'mode)
    [(:run) (rlet1 t (/. (- (current-jiffy) (~ T'start-time)) (jiffies-per-second))
              (set! (~ T'current-time) t))]
    [(:step) (~ T'current-time)]))

(define (step-scene-time! T :optional (delta-unit 1))
  (when (eq? (~ T'mode) :step)
    (set! (~ T'current-time)
          (max 0 (+ (~ T'current-time) (* delta-unit *stepper*))))))

(define (toggle-scene-time-mode! T)
  (case (~ T'mode)
    [(:run)  (set! (~ T'mode) :step)]
    [(:step) (set! (~ T'mode) :run)
     (set! (~ T'start-time)
           (- (current-jiffy)
              (round->exact (* (~ T'current-time) (jiffies-per-second)))))]))

(define *T* (make <scene-time>))

;; Drawing shape at the current scnee time.
;; This sample draws simple spiral motion.  You can change trajectory procedure
;; to try out diffent motions.
(define *vertical-speed* 0.3)   ; unit/sec
(define *horizontal-radius* 3.0)
(define *horizontal-speed* 2pi)  ; read/sec

(define (trajectory)
  (define T (current-scene-time *T*))
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
  (print "  R - Reset scene time")
  (print "  SPC - Toggle run/step mode")
  (print "  > - Step one unit forward")
  (print "  < - Step one unit back")
  (print "  ESC - Exit"))

(define (main args)
  (glut-init args)
  (simple-viewer-set-key! #f
                          #\r (^ _ (reset-scene-time! *T*))
                          #\space (^ _ (toggle-scene-time-mode! *T*))
                          #\< (^ _ (step-scene-time! *T* -1))
                          #\> (^ _ (step-scene-time! *T* 1))
                          #\? (^ _ (usage)))
  (simple-viewer-display (^[state]
                           (trajectory)
                           (glut-post-redisplay)))
  (simple-viewer-window 'demo)
  (simple-viewer-run)
  0)

;; Local variables:
;; mode: scheme
;; end:
