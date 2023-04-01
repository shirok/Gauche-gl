(use gl)
(use gl.glfw)
(add-load-path "." :relative)
(use load-shaders)

(define-constant Triangles 0)
(define-constant NumVAOs 1)

(define-constant ArrayBuffer 0)
(define-constant NumBuffers 1)

(define-constant vPosition 0)

(define VAOs)                           ;set by init
(define Buffers)                        ;set by init

(define-constant NumVertices 6)

(define (init)
  (define vertices
    '#f32(-0.90 -0.90                ; Triangle 1
           0.85 -0.90
          -0.90  0.85
           0.90 -0.85                ; Triangle 2
           0.90  0.90
          -0.85  0.90))

  (set! VAOs (gl-gen-vertex-arrays NumVAOs))
  (gl-bind-vertex-array (~ VAOs Triangles))

  (set! Buffers (gl-gen-buffers NumBuffers))
  (gl-bind-buffer GL_ARRAY_BUFFER (~ Buffers ArrayBuffer))
  (gl-buffer-data GL_ARRAY_BUFFER 0 vertices GL_STATIC_DRAW)

  ;; The book uses keypress.vert and keypress.frag, but they're the
  ;; same as triangles.* so we just reuse them.
  (receive (program shaders)
      (load-shaders `((,GL_VERTEX_SHADER   "shaders/triangles.vert")
                      (,GL_FRAGMENT_SHADER "shaders/triangles.frag")))
    (gl-use-program program))

  (gl-vertex-attrib-pointer vPosition 2 GL_FLOAT #f 0 0)
  (gl-enable-vertex-attrib-array vPosition)
  )

(define *mode* GL_FILL)

(define (key_cb window key scancode action mods)
  (when (eqv? action GLFW_PRESS)
    (cond
     [(eqv? key GLFW_KEY_M)
      (set! *mode* (if (eqv? *mode* GL_FILL) GL_LINE GL_FILL))
      (gl-polygon-mode GL_FRONT_AND_BACK *mode*)])))

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-bind-vertex-array (~ VAOs Triangles))
  (gl-draw-arrays GL_TRIANGLES 0 NumVertices)
  )

(define (resize_cb window width height)
  (gl-viewport 0 0 width height))

(define (main args)
  (glfw-init)

  (let1 window (glfw-create-window 800 600 "Keypress" #f #f)
    (glfw-set-window-size-callback window resize_cb)
    (glfw-set-key-callback window key_cb)
    (glfw-make-context-current window)
    (init)
    (until (glfw-window-should-close window)
      (disp)
      (glfw-swap-buffers window)
      (glfw-poll-events))
    (glfw-destroy-window window)
    (glfw-terminate))
  0)
