;; Example 6-2  Three-Dimensional Blending

(use gl)
(use gl.glut)

(define MAXZ 8.0)
(define MINZ -8.0)
(define ZINC 0.4)

(define *solid-z* MAXZ)
(define *transparent-z* MINZ)
(define *sphere-list* #f)
(define *cube-list* #f)

(define (init)
  (gl-material GL_FRONT GL_SPECULAR '#f32(1.0 1.0 1.0 0.15))
  (gl-material GL_FRONT GL_SHININESS 100.0)
  (gl-light GL_LIGHT0 GL_POSITION '#f32(0.5 0.5 1.0 0.0))

  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_DEPTH_TEST)

  (set! *sphere-list* (gl-gen-lists 1))
  (gl-new-list *sphere-list* GL_COMPILE)
  (glut-solid-sphere 0.4 16 16)
  (gl-end-list)

  (set! *cube-list* (gl-gen-lists 1))
  (gl-new-list *cube-list* GL_COMPILE)
  (glut-solid-cube 0.6)
  (gl-end-list)
  )

(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (gl-push-matrix)
  (gl-translate -0.15 -0.15 *solid-z*)
  (gl-material GL_FRONT GL_EMISSION '#f32(0.0 0.0 0.0 1.0))
  (gl-material GL_FRONT GL_DIFFUSE '#f32(0.75 0.75 0.0 1.0))
  (gl-call-list *sphere-list*)
  (gl-pop-matrix)

  (gl-push-matrix)
  (gl-translate 0.15 0.15 *transparent-z*)
  (gl-rotate 15.0 1.0 1.0 0.0)
  (gl-rotate 30.0 0.0 1.0 0.0)
  (gl-material GL_FRONT GL_EMISSION '#f32(0.0 0.3 0.3 0.6))
  (gl-material GL_FRONT GL_DIFFUSE '#f32(0.0 0.8 0.8 0.6))
  (gl-enable GL_BLEND)
  (gl-depth-mask #f)
  (gl-blend-func GL_SRC_ALPHA GL_ONE)
  (gl-call-list *cube-list*)
  (gl-depth-mask #t)
  (gl-disable GL_BLEND)
  (gl-pop-matrix)

  (glut-swap-buffers)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (if (<= w h)
      (gl-ortho -1.5 1.5 (* -1.5 (/ h w)) (* 1.5 (/ h w)) -10.0 10.0)
      (gl-ortho (* -1.5 (/ w h)) (* 1.5 (/ w h)) -1.5 1.5 -10.0 10.0))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  )

(define (animate)
  (if (or (<= *solid-z* MINZ) (>= *transparent-z* MAXZ))
      (glut-idle-func #f)
      (begin
        (dec! *solid-z* ZINC)
        (inc! *transparent-z* ZINC)
        (glut-post-redisplay)))
  )

(define (keyboard key x y)
  (cond
   ((or (= key (char->integer #\a))
        (= key (char->integer #\A)))
    (set! *solid-z* MAXZ)
    (set! *transparent-z* MINZ)
    (glut-idle-func animate))
   ((or (= key (char->integer #\r))
        (= key (char->integer #\R)))
    (set! *solid-z* MAXZ)
    (set! *transparent-z* MINZ)
    (glut-post-redisplay)))
  )

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size 500 500)
  (glut-create-window *program-name*)
  (init)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-display-func disp)
  (glut-main-loop)
  0)
