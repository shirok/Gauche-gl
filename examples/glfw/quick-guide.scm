;;;
;;; Port of GLFW tutorial
;;;

(use gauche.uvector)
(use gl)
(use gl.glfw)

(define *vertices*
  ;;      x     y     r     g     b
  '#f32( -0.6  -0.4   1.0   0.0   0.0
          0.6  -0.4   0.0   1.0   0.0
          0.0   0.6   0.0   0.0   1.0))

(define *vertex-shader-text*
  '("#version 110\n"
    "uniform mat4 MVP;\n"
    "attribute vec3 vCol;\n"
    "attribute vec2 vPos;\n"
    "varying vec3 color;\n"
    "void main()\n"
    "{\n"
    "  gl_Position = MVP * vec4(vPos, 0.0, 1.0);\n"
    "  color = vCol;\n"
    "}\n"))

(define *fragment-shader-text*
  '("#version 110\n"
    "varying vec3 color;\n"
    "void main()\n"
    "{\n"
    "  gl_FragColor = vec4(color, 1.0);\n"
    "}\n"))

(define (error-callback errno desc)
  (format (current-error-port) "Error: ~a\n" desc))

(define (key-callback window key scancode action modes)
  (when (and (eqv? key GLFW_KEY_ESCAPE)
             (eqv? action GLFW_PRESS))
    (glfw-set-window-should-close window #t)))

(define (main args)
  (glfw-set-error-callback error-callback)

  (unless (glfw-init)
    (exit 1))

  (glfw-window-hint GLFW_CONTEXT_VERSION_MAJOR 2)
  (glfw-window-hint GLFW_CONTEXT_VERSION_MINOR 0)

  (let1 window (glfw-create-window 640 480 "Simple example" #f #f)
    (unless window
      (glfw-terminate)
      (exit 1))
    (glfw-set-key-callback window key-callback)

    (glfw-make-context-current window)
    (glfw-swap-interval 1)

    (let ([vertex-buffer (gl-gen-buffers 1)]
          [vertex-shader (gl-create-shader-object-arb GL_VERTEX_SHADER)]
          [fragment-shader (gl-create-shader-object-arb GL_FRAGMENT_SHADER)]
          [program (gl-create-program-object-arb)]
          [m (make-matrix4f)]
          [p (make-matrix4f)]
          [mvp (make-matrix4f)])
      (gl-bind-buffer GL_ARRAY_BUFFER (u32vector-ref vertex-buffer 0))
      (gl-buffer-data GL_ARRAY_BUFFER 0 *vertices* GL_STATIC_DRAW)

      (gl-shader-source-arb vertex-shader *vertex-shader-text*)
      (gl-compile-shader-arb vertex-shader)

      (gl-shader-source-arb fragment-shader *fragment-shader-text*)
      (gl-compile-shader-arb fragment-shader)

      (gl-attach-object-arb program vertex-shader)
      (gl-attach-object-arb program fragment-shader)
      (gl-link-program-arb program)

      (let ([mvp-location (gl-get-uniform-location-arb program "MVP")]
            [vpos-location (gl-get-attrib-location-arb program "vPos")]
            [vcol-location (gl-get-attrib-location-arb program "vCol")])
        (gl-enable-vertex-attrib-array-arb vpos-location)
        (gl-vertex-attrib-pointer-arb vpos-location 2 *vertices*
                                  #f (* 5 4) 0)
        (gl-vertex-attrib-pointer-arb vcol-location 3 *vertices*
                                  #f (* 5 4) (* 2 4))

        (until (glfw-window-should-close window)
          (receive (w h) (glfw-get-framebuffer-size window)
            (let ([ratio (/ w h)])
              (gl-viewport 0 0 w h)
              (gl-clear GL_COLOR_BUFFER_BIT)

              (euler-angle->matrix4f! mvp 0 0 (glfw-get-time))
              (ortho->matrix4f! p (- ratio) ratio -1.0 1.0 1.0 -1.0)
              (matrix4f-mul! mvp p)

              (gl-use-program-object-arb program)
              (gl-uniform-matrix4-arb mvp-location #f
                                      (matrix4f->f32vector mvp))
              (gl-draw-arrays GL_TRIANGLES 0 3)

              (glfw-swap-buffers window)
              (glfw-poll-events))))

        (glfw-destroy-window window)

        (glfw-terminate)
        (exit EXIT_SUCCESS)))
    ))
