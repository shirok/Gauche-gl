;;;
;;; Minimal example
;;;

(use gauche.uvector)
(use gl)
(use gl.glfw)

(define (error-callback errno desc)
  (format (current-error-port) "Error: ~a\n" desc))

(define (key-callback window key scancode action modes)
  (when (and (eqv? key GLFW_KEY_ESCAPE)
             (eqv? action GLFW_PRESS))
    (glfw-set-window-should-close window #t)))

(define (main args)
  (glfw-set-error-callback error-callback)

  (unless (glfw-init) (exit 1))

  (let1 window (glfw-create-window 400 400 "Simple example" #f #f)
    (unless window
      (glfw-terminate)
      (exit 1))
    (glfw-set-key-callback window key-callback)

    (glfw-make-context-current window)
    (glfw-swap-interval 1)

    (gl-clear-color 0.0 0.0 0.0 0.0)
    (gl-shade-model GL_FLAT)

    (let loop ()
      (unless (glfw-window-should-close window)
        (receive (w h) (glfw-get-framebuffer-size window)
          (gl-viewport 0 0 w h)
          (gl-matrix-mode GL_PROJECTION)
          (gl-load-identity)
          (gl-ortho -50.0 50.0 -50.0 50.0 -1.0 1.0)
          (gl-matrix-mode GL_MODELVIEW)
          (gl-load-identity)

          (gl-clear GL_COLOR_BUFFER_BIT)
          (gl-push-matrix)
          (gl-rotate (* (glfw-get-time) 90) 0.0 0.0 1.0)
          (gl-color '#f32(1.0 1.0 1.0))
          (gl-rect '#f32(-25.0 -25.0) '#f32(25.0 25.0))
          (gl-pop-matrix)

          (glfw-swap-buffers window)
          (glfw-poll-events))
        (loop)))

    (glfw-destroy-window window)
    (glfw-terminate)
    (exit 0)
    ))
