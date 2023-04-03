;; Example 2-17 Using buffer objects with vertex data

(use gl)
(use gl.glut)
(use gauche.uvector)

(define-constant VERTICES 0)
(define-constant INDICES 1)
(define-constant NUM_BUFFERS 2)

(define *buffers* #f) ; set by init

(define *vertices*
  '#f32(-1.0 -1.0 -1.0
         1.0 -1.0 -1.0
         1.0  1.0 -1.0
        -1.0  1.0 -1.0
        -1.0 -1.0  1.0
         1.0 -1.0  1.0
         1.0  1.0  1.0
        -1.0  1.0  1.0))

(define *indices*
  '#u8(0 1 2 3
       4 7 6 3
       0 4 5 1
       3 2 6 7
       0 3 7 4
       1 5 6 2))

(define *angle* 0)

(define (init)
  (set! *buffers* (gl-gen-buffers NUM_BUFFERS))

  (gl-bind-buffer GL_ARRAY_BUFFER (~ *buffers* VERTICES))
  (gl-buffer-data GL_ARRAY_BUFFER
                  0
                  *vertices*
                  GL_STATIC_DRAW)
  (gl-vertex-pointer 3 #f 0 0 GL_FLOAT)
  (gl-enable-client-state GL_VERTEX_ARRAY)

  (gl-bind-buffer GL_ELEMENT_ARRAY_BUFFER (~ *buffers* INDICES))
  (gl-buffer-data GL_ELEMENT_ARRAY_BUFFER
                  0
                  *indices*
                  GL_STATIC_DRAW)
  )

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color '#f32(1.0 1.0 1.0))
  (gl-load-identity)
  (gl-translate 0.0 0.0 -5.0)
  (gl-rotate *angle* 1.0 1.0 1.0)
  (gl-draw-elements GL_QUADS (u8vector-length *indices*) GL_UNSIGNED_BYTE)
  (let1 e (gl-get-error)
    (unless (zero? e) (print e)))
  (gl-flush))

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-frustum -1.0 1.0 -1.0 1.0 1.5 20.0)
  (gl-matrix-mode GL_MODELVIEW))

(define (keyboard key x y)
  (when (= key 27) (exit 0)))

(define (timer _)
  (inc! *angle*)
  (glut-post-redisplay)
  (glut-timer-func 20 timer 0))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size 350 350)
  (glut-init-window-position 100 100)
  (glut-create-window *program-name*)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)
  (init)
  (glut-display-func disp)
  (glut-timer-func 20 timer 0)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)
