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
;;; $Id: ogl2brick.scm,v 1.4 2005-06-10 11:27:16 shirok Exp $

(use srfi-1)
(use gauche.uvector)
(use gl)
(use gl.glut)
(use math.const)
(use util.match)
(use file.util)

;; Movement variables
(define *fxdiff* 206)
(define *fydiff*  16)
(define *fzdiff*  10)
(define *xlastincr* 0)
(define *ylastincr* 0)
(define *fxinertia* -0.5)
(define *fyinertia* 0)
(define *fxinertiaold* 0)
(define *fyinertiaold* 0)
(define *fscale*   1.0)
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

(define *rotl* pi/180)
(define *lasttime* 0)

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

;; Models to render
;;   a circular list of thunks

(define *models*
  (circular-list (lambda () (glut-solid-teapot 0.6))
                 (lambda () (glut-solid-torus 0.2 0.6 64 64))
                 (lambda () (glut-solid-sphere 0.6 64 64))
                 (lambda () (draw-cube))))

;;;
;;; GLUT glue
;;;

(define (display-proc)
  (gl-load-identity)
  (gl-translate 0.0 0.0 -5.0)

  (gl-rotate *fydiff* 1 0 0)
  (gl-rotate *fxdiff* 0 1 0)
  (gl-rotate *fzdiff* 0 0 1)

  (gl-scale *fscale* *fscale* *fscale*)

  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  ((car *models*))

  (gl-flush)
  (glut-swap-buffers)
  )

(define (play-proc)
  (let1 thistime (glut-get GLUT_ELAPSED_TIME)
    (inc! *rotl* (* (- thistime *lasttime*) -0.001))
    (set! *lasttime* thistime)
    (glut-post-redisplay)))

(define (key-proc key x y)
  (cond
   ((eqv? key (char->integer #\b))
    (next-clear-color))
   ((eqv? key (char->integer #\q))
    (exit))
   ((eqv? key (char->integer #\t))
    (pop! *models*))
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
    (print "t - Toggle among models to render")
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
;;; draw-cube (original source is draw.c)
;;;

(define (draw-cube)
  (let* ((size  1.0)
         (scale 0.2)
         (delta 0.1)
         (-size (- size))
         (A (f32vector  size  size (+ (*  size scale) delta)))
         (B (f32vector  size  size (+ (* -size scale) delta)))
         (C (f32vector  size -size (* -size scale)))
         (D (f32vector  size -size (*  size scale)))
         (E (f32vector -size  size (+ (*  size scale) delta)))
         (F (f32vector -size  size (+ (* -size scale) delta)))
         (G (f32vector -size -size (* -size scale)))
         (H (f32vector -size -size (*  size scale)))
         (I '#f32( 1.0  0.0  0.0))
         (K '#f32(-1.0  0.0  0.0))
         (L '#f32( 0.0  0.0 -1.0))
         (M '#f32( 0.0  0.0  1.0))
         (N '#f32( 0.0  1.0  0.0))
         (O '#f32( 0.0 -1.0  0.0))
         )
    (gl-begin GL_QUADS)

    (for-each
     (match-lambda
       ((n v0 v1 v2 v3)
        (gl-normal n)
        (gl-tex-coord 1 1) (gl-vertex v0)
        (gl-tex-coord 0 1) (gl-vertex v1)
        (gl-tex-coord 0 0) (gl-vertex v2)
        (gl-tex-coord 1 0) (gl-vertex v3)))
     `((,I ,D ,C ,B ,A)
       (,K ,G ,H ,E ,F)
       (,L ,C ,G ,F ,B)
       (,M ,H ,D ,A ,E)
       (,N ,E ,A ,B ,F)
       (,O ,G ,C ,D ,H)))

    (gl-end)
    ))

;;;
;;; shader stuff (original source is shader.c)
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

(define (install-brick-shaders vsh fsh)
  ;; Create a vertex shader object and a fragment shader object
  (let* ((brickvs (gl-create-shader-object-arb GL_VERTEX_SHADER_ARB))
         (brickfs (gl-create-shader-object-arb GL_FRAGMENT_SHADER_ARB))
         (vert-compiled 1)
         (frag-compiled 1)
         (linked #f))
    ;; Load source code strings into shaders
    (gl-shader-source-arb brickvs (list vsh))
    (gl-shader-source-arb brickfs (list fsh))
    ;; Compile the brick vertex shader, and print out
    ;; the compiler log file.
    (gl-compile-shader-arb brickvs)
    (print-opengl-error) ;; Check for OpenGL errors
    (set! vert-compiled
          (gl-get-object-parameter-arb brickvs GL_OBJECT_COMPILE_STATUS_ARB))
    (print-info-log brickvs)
    ;; Compile the brick fragment shader, and print out
    ;; the compiler log file.
    (gl-compile-shader-arb brickfs)
    (print-opengl-error)
    (set! frag-compiled
          (gl-get-object-parameter-arb brickfs GL_OBJECT_COMPILE_STATUS_ARB))
    (print-info-log brickfs)

    (if (or (zero? vert-compiled) (zero? frag-compiled))
      #f ;; failure
      ;; Create a program object and attach the two compiled shaders
      (let1 brickprog (gl-create-program-object-arb)
        (gl-attach-object-arb brickprog brickvs)
        (gl-attach-object-arb brickprog brickfs)
        ;; Link the program object and print out the info log
        (gl-link-program-arb brickprog)
        (print-opengl-error)
        (set! linked
              (gl-get-object-parameter-arb brickprog GL_OBJECT_LINK_STATUS_ARB))
        (print-info-log brickprog)
        (if (zero? linked)
          #f ;; failure
          (begin
            ;; Install program object as part of current state
            (gl-use-program-object-arb brickprog)
            (gl-uniform3-arb (get-uniloc brickprog "BrickColor")
                             1.0 0.3 0.2)
            (gl-uniform3-arb (get-uniloc brickprog "MortarColor")
                             0.85 0.86 0.84)
            (gl-uniform2-arb (get-uniloc brickprog "BrickSize")
                             0.30 0.15)
            (gl-uniform2-arb (get-uniloc brickprog "BrickPct")
                             0.90 0.85)
            (gl-uniform3-arb (get-uniloc brickprog "LightPosition")
                             0.0 0.0 4.0)
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
  (let* ((window (glut-create-window "3Dlabs Brick Shader")))

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

    (gl-depth-func GL_LESS)
    (gl-enable GL_DEPTH_TEST)
    (next-clear-color)

    (key-proc (char->integer #\?) 0 0) ;; display help

    (let ((vs (file->string "brick.vert"))
          (fs (file->string "brick.frag")))
      (and (install-brick-shaders vs fs)
           (glut-main-loop)))
    0))
