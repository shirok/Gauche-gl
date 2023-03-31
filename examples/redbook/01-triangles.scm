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

  (set! Buffers (gl-create-buffers NumBuffers))
  (gl-named-buffer-storage (~ Buffers ArrayBuffer) 0 vertices 0)

  (receive (program shaders)
      (load-shaders `((,GL_VERTEX_SHADER   "shaders/triangles.vert")
                      (,GL_FRAGMENT_SHADER "shaders/triangles.frag")))
    (gl-use-program program))

  (set! VAOs (gl-gen-vertex-arrays NumVAOs))
  (gl-bind-vertex-array (~ VAOs Triangles))
  (gl-bind-buffer GL_ARRAY_BUFFER (~ Buffers ArrayBuffer))
  (gl-vertex-attrib-pointer vPosition 2 GL_FLOAT #f 0 0)
  (gl-enable-vertex-attrib-array vPosition)
  )

(define (disp)
  (gl-clear-buffer GL_COLOR 0 '#f32(0 0 0 0))
  (gl-bind-vertex-array (~ VAOs Triangles))
  (gl-draw-arrays GL_TRIANGLES 0 NumVertices)
  )

(define (main args)
  (glfw-init)

  (let1 window (glfw-create-window 640 480 "Triangles" #f #f)
    (glfw-make-context-current window)

    ;; gl3wInit() is not necessary - loading GL is done by Gauche-gl
    (init)
    (until (glfw-window-should-close window)
      (disp)
      (glfw-swap-buffers window)
      (glfw-poll-events))
    (glfw-destroy-window window)
    (glfw-terminate))
  0)
