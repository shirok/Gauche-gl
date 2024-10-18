;;;
;;; simple/scene-time.scm - scene time management
;;;
;;;   Copyright (c) 2024  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; This module manages the time in the scene.  Useful if
;; the scene is time-dependent (e.g. animation)

(define-module gl.simple.scene-time
  (use scheme.time)
  (export <scene-time>
          default-scene-time
          reset-scene-time!
          current-scene-time
          toggle-scene-time-mode!
          step-scene-time!
          scene-time-speed-factor
          scene-time-stepper))
(select-module gl.simple.scene-time)

;; Represents absolute time in the scene
;;  In 'run' mode, current-scene-time consider the real time passed.
;;  in 'step' mode, current-scene-time is controlled by program.
(define-class <scene-time> ()
  ((mode :init-keyword :mode :init-value :run)
   (stepper :init-keyword :stepper :init-value 1.0) ;seconds
   (speed-factor :init-keyword :speed-factor :init-value 1.0)

   (prev-jiffy :init-form (current-jiffy)) ;jiffies
   (current-time :init-value 0)))       ;seconds

(define *default-scene-time* (make <scene-time>))

(define (default-scene-time) *default-scene-time*)

(define reset-scene-time!
  (case-lambda
    [() (reset-scene-time! *default-scene-time* 0.0)]
    [(arg)
     (typecase arg
       [<scene-time> (reset-scene-time! arg 0.0)]
       [<real> (reset-scene-time! *default-scene-time* arg)])]
    [(T time-in-sec)
     (set! (~ T'prev-jiffy) (current-jiffy))
     (set! (~ T'current-time) time-in-sec)]))

(define current-scene-time
  (case-lambda
    [() (current-scene-time *default-scene-time*)]
    [(T)
     (case (~ T'mode)
       [(:run) (let* ([now (current-jiffy)]
                      [dt (/. (* (- now (~ T'prev-jiffy)) (~ T'speed-factor))
                              (jiffies-per-second))])
                 (set! (~ T'prev-jiffy) now)
                 (inc! (~ T'current-time) dt)
                 (~ T'current-time))]
       [(:step) (~ T'current-time)])]))

(define step-scene-time!
  (case-lambda
    [() (step-scene-time! *default-scene-time*)]
    [(arg)
     (typecase arg
       [<scene-time> (step-scene-time! arg 1)]
       [<real> (step-scene-time! *default-scene-time* arg)])]
    [(T delta-unit)
     (when (eq? (~ T'mode) :step)
       (set! (~ T'current-time)
             (max 0 (+ (~ T'current-time) (* delta-unit (~ T'stepper))))))]))

(define toggle-scene-time-mode!
  (case-lambda
    [() (toggle-scene-time-mode! *default-scene-time*)]
    [(T)
     (case (~ T'mode)
       [(:run)  (set! (~ T'mode) :step)]
       [(:step) (set! (~ T'mode) :run) (set! (~ T'prev-jiffy) (current-jiffy))])]))

(define scene-time-speed-factor
  (getter-with-setter
   (case-lambda
     [()  (scene-time-speed-factor *default-scene-time*)]
     [(T) (~ T'speed-factor)])
   (case-lambda
     [(val) (set! (scene-time-speed-factor *default-scene-time*) val)]
     [(T val) (set! (~ T'speed-factor) val)])))

(define scene-time-stepper
  (getter-with-setter
   (case-lambda
     [()  (scene-time-stepper *default-scene-time*)]
     [(T) (~ T'stepper)])
   (case-lambda
     [(val) (set! (scene-time-stepper *default-scene-time*) val)]
     [(T val) (set! (~ T'stepper) val)])))
