;;;
;;; ogl2brick - OpenGL shading language demo
;;;
;;; This is a pretty straightforward port of the C code provided
;;; by 3DLabs Inc under the BSD-license.  The original copyright
;;; notice follows.
;;;
#|
/************************************************************************
 *                                                                      *
 *              Copyright (C) 2002-2004  3Dlabs Inc. Ltd.               *
 *                                                                      *
 *                        All rights reserved.                          *
 *                                                                      *
 * Redistribution and use in source and binary forms, with or without   *
 * modification, are permitted provided that the following conditions   *
 * are met:                                                             *
 *                                                                      *
 *     Redistributions of source code must retain the above copyright   *
 *     notice, this list of conditions and the following disclaimer.    *
 *                                                                      *
 *     Redistributions in binary form must reproduce the above          *
 *     copyright notice, this list of conditions and the following      *
 *     disclaimer in the documentation and/or other materials provided  *
 *     with the distribution.                                           *
 *                                                                      *
 *     Neither the name of the 3Dlabs nor the names of its              *
 *     contributors may be used to endorse or promote products derived  *
 *     from this software without specific prior written permission.    *
 *                                                                      *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS  *
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT    *
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS    *
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE       *
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,  *
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, *
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;     *
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER     *
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT   *
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN    *
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE      *
 * POSSIBILITY OF SUCH DAMAGE.                                          *
 *                                                                      *
/************************************************************************/
|#
;;; $Id: ogl2particle.scm,v 1.4 2005-06-10 11:27:17 shirok Exp $

(use srfi-1)
(use srfi-27)
(use gauche.uvector)
(use gl)
(use gl.glut)
(use file.util)
(use math.const)

;; flags for doing animation
(define *particle-time* 0.0)

(define *program-object* #f)

;; Movement variables
(define *fxdiff* 206.0)
(define *fydiff* 16.0)
(define *fzdiff* 10.0)
(define *xlastincr* 0)
(define *ylastincr* 0)
(define *fxinertia* -0.5)
(define *fyinertia* 0)
(define *fxinertiaold* 0)
(define *fyinertiaold* 0)
(define *fscale*   0.25)
(define *ftime*    0)
(define *xlast*    -1)
(define *ylast*    -1)
(define *modifiers* 0)
(define *rotate*   #t)

;; rotation defines
(define-constant INERTIA_THRESHOLD 1.0)
(define-constant INERTIA_FACTOR    0.5)
(define-constant SCALE_FACTOR      0.01)
(define-constant SCALE_INCREMENT   0.5)
(define-constant TIMER_FREQUENCY_MILLIS 20)

;; extra uniform arrays
(define-constant VELOCITY_ARRAY 4)
(define-constant START_TIME_ARRAY 5)

(define (print-opengl-error)
  (let loop ((err (gl-get-error))
             (status 0))
    (if (= err GL_NO_ERROR)
      status
      (begin
        (format "glError: ~s~%" (glu-error-string err))
        (loop (gl-get-error) 1)))))

(define next-clear-color
  (let1 color 0
    (lambda ()
      (case (modulo color 3)
        ((0) (gl-clear-color 0.0 0.0 0.0 1.0))
        ((1) (gl-clear-color 0.2 0.2 0.3 1.0))
        ((2) (gl-clear-color 0.7 0.7 0.7 1.0)))
      (inc! color))))

;;
;; GLUT glue
;;

(define (display-proc)
  (gl-load-identity)
  (gl-translate 0.0 0.0 -5.0)

  (gl-rotate *fydiff* 1 0 0)
  (gl-rotate *fxdiff* 0 1 0)
  (gl-rotate *fzdiff* 0 0 1)

  (gl-scale *fscale* *fscale* *fscale*)

  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (draw-points)

  (gl-flush)
  (glut-swap-buffers)
  )

(define (update-anim)
  (let1 location (get-uniloc *program-object* "Time")
    (inc! *particle-time* 0.002)
    (when (> *particle-time* 15.0)
      (set! *particle-time* 0.0))
    (gl-uniform1-arb location *particle-time*)
    (print-opengl-error)))

(define (play-proc)
  (let1 thistime (glut-get GLUT_ELAPSED_TIME)
    (update-anim)
    (print-opengl-error)
    (glut-post-redisplay)
    (print-opengl-error)))

(define (key-proc key x y)
  (set! *particle-time* 0)
  (cond
   ((eqv? key (char->integer #\b))
    (next-clear-color))
   ((eqv? key (char->integer #\q))
    (exit))
   ((eqv? key (char->integer #\space))
    (set! *rotate* (not *rotate*))
    (if (not *rotate*)
      (begin
        (set! *fxinertiaold* *fxinertia*)
        (set! *fyinertiaold* *fyinertia*))
      (begin
        (set! *fxinertia* *fxinertiaold*)
        (set! *fyinertia* *fyinertiaold*)
        ;; To prevent confusion, force some rotation
        (when (and (zero? *fxinertia*) (zero? *fyinertia*))
          (set! *fxinertia* -0.5)))))
   ((eqv? key (char->integer #\+))
    (inc! *fscale* SCALE_INCREMENT))
   ((eqv? key (char->integer #\-))
    (dec! *fscale* SCALE_INCREMENT))
   (else
    (print "Keyboard commands:\n")
    (print "b - Toggle among background clear colors")
    (print "q - Quit")
    (print "? - Help")
    (print "<home>     - reset zoom and rotation")
    (print "<space> or <click>        - stop rotation")
    (print "<+>, <-> or <ctrl + drag> - zoom model")
    (print "<arrow keys> or <drag>    - rotate model"))
   ))

(define (timer-proc value)
  (inc! *ftime* 0.01)
  (when *rotate*
    (inc! *fxdiff* *fxinertia*)
    (inc! *fydiff* *fyinertia*))
  (glut-timer-func TIMER_FREQUENCY_MILLIS timer-proc 0))

(define (mouse-proc button state x y)
  (set! *modifiers* (glut-get-modifiers))
  (when (= button GLUT_LEFT_BUTTON)
    (if (= state GLUT_UP)
      (begin
        (set! *xlast* -1)
        (set! *ylast* -1)
        (when (> *xlastincr* INERTIA_THRESHOLD)
          (set! *fxinertia*
                (* (- *xlastincr* INERTIA_THRESHOLD) INERTIA_FACTOR)))
        (when (> (- *xlastincr*) INERTIA_THRESHOLD)
          (set! *fxinertia*
                (* (+ *xlastincr* INERTIA_THRESHOLD) INERTIA_FACTOR)))
        (when (> *ylastincr* INERTIA_THRESHOLD)
          (set! *fyinertia*
                (* (- *ylastincr* INERTIA_THRESHOLD) INERTIA_FACTOR)))
        (when (> (- *ylastincr*) INERTIA_THRESHOLD)
          (set! *fyinertia*
                (* (+ *ylastincr* INERTIA_THRESHOLD) INERTIA_FACTOR)))
        )
      (begin
        (set! *fxinertia* 0)
        (set! *fyinertia* 0)))
    (set! *xlastincr* 0)
    (set! *ylastincr* 0))
  )

(define (motion-proc x y)
  (unless (and (= *xlast* -1) (= *ylast* -1))
    (set! *xlastincr* (- x *xlast*))
    (set! *ylastincr* (- y *ylast*))
    (if (logand *modifiers* GLUT_ACTIVE_CTRL)
      (unless (= *xlast* -1)
        (inc! *fzdiff* *xlastincr*)
        (inc! *fscale* (* *ylastincr* SCALE_FACTOR)))
      (unless (= *xlast* -1)
        (inc! *fxdiff* *xlastincr*)
        (inc! *fydiff* *ylastincr*))))
  (set! *xlast* x)
  (set! *ylast* y))

(define (reshape-proc w h)
  (let ((vp 0.8)
        (aspect (/ w h)))
    (gl-viewport 0 0 w h)
    (gl-matrix-mode GL_PROJECTION)
    (gl-load-identity)

    (gl-frustum (- vp) vp (/ (- vp) aspect) (/ vp aspect) 3 10.0)

    (gl-matrix-mode GL_MODELVIEW)
    (gl-load-identity)
    (gl-translate 0.0 0.0 -5.0)))

(define (special-proc key x y)
  (cond
   ((= key GLUT_KEY_HOME)
    (set! *fxdiff* 0)
    (set! *fydiff* 35)
    (set! *fzdiff* 0)
    (set! *xlastincr* 0)
    (set! *ylastincr* 0)
    (set! *fxinertia* -0.5)
    (set! *fyinertia* 0)
    (set! *fscale* 1.0))
   ((= key GLUT_KEY_LEFT)
    (dec! *fxdiff*))
   ((= key GLUT_KEY_RIGHT)
    (inc! *fxdiff*))
   ((= key GLUT_KEY_UP)
    (dec! *fydiff*))
   ((= key GLUT_KEY_DOWN)
    (inc! *fydiff*))
   ))

;;;
;;; create-points and draw-points (original source is draw.c)
;;;

(define-values
  (create-points draw-points)
  (let ((array-width #f)
        (array-height #f)
        (verts #f)
        (colors #f)
        (velocities #f)
        (start-times #f))

    (define (create-points w h)
      (set! verts  (make-f32vector (* w h 3)))
      (set! colors (make-f32vector (* w h 3)))
      (set! velocities (make-f32vector (* w h 3)))
      (set! start-times (make-f32vector (* w h)))

      (do ((i (- (/ 0.5 w) 0.5) (+ i (/ w)))
           (n 0 (+ n 3))
           (m 0 (+ m 1)))
          ((>= i 0.5))
        (do ((j (- (/ 0.5 h) 0.5) (+ j (/ h)))
             (n n (+ n 3))
             (m m (+ m 1)))
            ((>= j 0.5))
          (set! (ref verts n)       i)
          (set! (ref verts (+ n 1)) 0.0)
          (set! (ref verts (+ n 2)) j)

          (set! (ref colors n)       (/ (+ (random-real) 1) 2))
          (set! (ref colors (+ n 1)) (/ (+ (random-real) 1) 2))
          (set! (ref colors (+ n 2)) (/ (+ (random-real) 1) 2))

          (set! (ref velocities n)       (+ (random-real) 3.0))
          (set! (ref velocities (+ n 1)) (* (random-real) 10.0))
          (set! (ref velocities (+ n 2)) (+ (random-real) 3.0))

          (set! (ref start-times m) (* (random-real) 10.0))
          )
        )

      (set! array-width w)
      (set! array-height h))

    (define (draw-points)
      (gl-point-size 2.0)
      (gl-vertex-pointer 3 verts)
      (gl-color-pointer 3 colors)
      (gl-vertex-attrib-pointer-arb VELOCITY_ARRAY 3 velocities)
      (gl-vertex-attrib-pointer-arb START_TIME_ARRAY 1 start-times)
      (gl-enable-client-state GL_VERTEX_ARRAY)
      (gl-enable-client-state GL_COLOR_ARRAY)
      (gl-enable-vertex-attrib-array-arb VELOCITY_ARRAY)
      (gl-enable-vertex-attrib-array-arb START_TIME_ARRAY)

      (gl-draw-arrays GL_POINTS 0 (* array-width array-height))

      (gl-disable-client-state GL_VERTEX_ARRAY)
      (gl-disable-client-state GL_COLOR_ARRAY)
      (gl-disable-vertex-attrib-array-arb VELOCITY_ARRAY)
      (gl-disable-vertex-attrib-array-arb START_TIME_ARRAY)
      )

    (values create-points draw-points)))

;;;
;;; shader-stuff (original source in shader.c)
;;;

(define (get-uniloc program name)
  (let1 loc (gl-get-uniform-location-arb program name)
    (when (negative? loc)
      (error "No such uniform:" name))
    (print-opengl-error)
    loc))

(define (print-info-log obj)
  (print-opengl-error)
  (format #t "InfoLog:\n~a\n\n" (gl-get-info-log-arb obj))
  (print-opengl-error))

(define (install-particle-shaders vsh fsh)
  ;; Create a vertex shader object and a fragment shader object
  (let* ((vs (gl-create-shader-object-arb GL_VERTEX_SHADER_ARB))
         (fs (gl-create-shader-object-arb GL_FRAGMENT_SHADER_ARB))
         (vert-compiled 1)
         (frag-compiled 1)
         (linked #f))
    ;; Load source code strings into shaders
    (gl-shader-source-arb vs (list vsh))
    (gl-shader-source-arb fs (list fsh))
    ;; Compile the brick vertex shader, and print out
    ;; the compiler log file.
    (gl-compile-shader-arb vs)
    (print-opengl-error) ;; Check for OpenGL errors
    (set! vert-compiled
          (gl-get-object-parameter-arb vs GL_OBJECT_COMPILE_STATUS_ARB))
    (print-info-log vs)
    ;; Compile the brick fragment shader, and print out
    ;; the compiler log file.
    (gl-compile-shader-arb fs)
    (print-opengl-error)
    (set! frag-compiled
          (gl-get-object-parameter-arb fs GL_OBJECT_COMPILE_STATUS_ARB))
    (print-info-log fs)

    (if (or (zero? vert-compiled) (zero? frag-compiled))
      #f ;; failure
      ;; Create a program object and attach the two compiled shaders
      (let1 progobj (gl-create-program-object-arb)
        (set! *program-object* progobj)
        (gl-attach-object-arb progobj vs)
        (gl-attach-object-arb progobj fs)
        ;; Bind generic attribute indices
        (gl-bind-attrib-location-arb progobj VELOCITY_ARRAY "Velocity")
        (gl-bind-attrib-location-arb progobj START_TIME_ARRAY "StartTime")
        ;; Link the program object and print out the info log
        (gl-link-program-arb progobj)
        (print-opengl-error)
        (set! linked
              (gl-get-object-parameter-arb progobj GL_OBJECT_LINK_STATUS_ARB))
        (print-info-log progobj)
        (if (zero? linked)
          #f ;; failure
          (begin
            ;; Install program object as part of current state
            (gl-use-program-object-arb progobj)
            (gl-uniform4-arb (get-uniloc progobj "Background")
                               0.0 0.0 0.0 1.0)
            (print-opengl-error)
            (gl-uniform1-arb (get-uniloc progobj "Time") -5.0)
            (print-opengl-error)
            #t ;; success
            ))
        )))
  )

;;;
;;; main
;;;

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_RGB GLUT_DEPTH GLUT_DOUBLE))
  (glut-init-window-size 500 500)
  (let* ((window (glut-create-window
                  "3Dlabs OpenGL Shading Language Particle System Demo")))

    (unless (gl-extension-available? 'GL_ARB_shader_objects
                                     'GL_ARB_fragment_shader
                                     'GL_ARB_vertex_shader
                                     'GL_ARB_shading_language_100)
      (error "OpenGL Shading Language extensions not available"))

    (glut-idle-func play-proc)
    (glut-display-func display-proc)
    (glut-keyboard-func key-proc)
    (glut-reshape-func reshape-proc)
    (glut-motion-func motion-proc)
    (glut-mouse-func mouse-proc)
    (glut-special-func special-proc)
    (glut-timer-func TIMER_FREQUENCY_MILLIS timer-proc 0)

    (create-points 100 100)

    (gl-depth-func GL_LESS)
    (gl-enable GL_DEPTH_TEST)
    (next-clear-color)

    (key-proc (char->integer #\?) 0 0) ;; display help

    (let ((vs (file->string "particle.vert"))
          (fs (file->string "particle.frag")))
      (and (install-particle-shaders vs fs)
           (glut-main-loop)))
    0))
