(use gauche.uvector)
(use gl)
(use gl.glfw)
(add-load-path "." :relative)
(use load-shaders)

(define *aspect* 1.0)
(define *vao*)
(define *vbo*)
(define *ebo*)

(define *render-prog*)
(define *render-model-matrix-loc*)
(define *render-projection-matrix-loc*)

(define *vertex-positions*
  '#f32(-1.0 -1.0  0.0  1.0
         1.0 -1.0  0.0  1.0
         1.0  0.0  1.0  1.0
         0.0  1.0  1.0  1.0))

(define *vertex-colors*
  '#f32( 1.0  1.0  1.0  1.0
         1.0  1.0  0.0  1.0
         1.0  0.0  1.0  1.0
         0.0  1.0  1.0  1.0))

(define (init)
  (receive (program shaders)
      (load-shaders `((,GL_VERTEX_SHADER   "shaders/primitive_restart.vert")
                      (,GL_FRAGMENT_SHADER "shaders/primitive_restart.frag")))
    (gl-use-program program)
    (set! *render-prog* program)
    (set! *render-model-matrix-loc*
          (gl-get-uniform-location program "model_matrix"))
    (set! *render-projection-matrix-loc*
          (gl-get-uniform-location program "projection_matrix")))

  (set! *ebo* (gl-gen-buffers 1))
  (gl-bind-buffer GL_ELEMENT_ARRAY_BUFFER (~ *ebo* 0))
  (gl-buffer-data GL_ELEMENT_ARRAY_BUFFER 0 '#u16(0 1 2) GL_STATIC_DRAW)

  (set! *vao* (gl-gen-vertex-arrays 1))
  (gl-bind-vertex-array (~ *vao* 0))

  (set! *vbo* (gl-gen-vertex-arrays 1))
  (gl-bind-buffer GL_ARRAY_BUFFER (~ *vbo* 0))
  (gl-buffer-data GL_ARRAY_BUFFER
                  (+ (uvector-size *vertex-positions*)
                     (uvector-size *vertex-colors*))
                  #f GL_STATIC_DRAW)
  (gl-buffer-sub-data GL_ARRAY_BUFFER
                      0
                      (uvector-size *vertex-positions*)
                      *vertex-positions*)
  (gl-buffer-sub-data GL_ARRAY_BUFFER
                      (uvector-size *vertex-positions*)
                      (uvector-size *vertex-colors*)
                      *vertex-colors*)

  (gl-vertex-attrib-pointer 0 4 GL_FLOAT #f 0 0)
  (gl-vertex-attrib-pointer 1 4 GL_FLOAT #f 0 (uvector-size *vertex-positions*))
  (gl-enable-vertex-attrib-array 0)
  (gl-enable-vertex-attrib-array 1)
  )

(define (disp window)
  (define mat (make-matrix4f))

  (gl-enable GL_CULL_FACE)
  (gl-disable GL_DEPTH_TEST)

  (gl-clear-buffer GL_COLOR 0 '#f32(0 0 0 0))

  (gl-use-program *render-prog*)

  (frustum->matrix4f! mat -1.0 1.0 (- *aspect*) *aspect* 1.0 500.0)
  (gl-uniform-matrix4 *render-projection-matrix-loc* #f mat)

  (gl-bind-vertex-array (~ *vao* 0))
  (gl-bind-buffer GL_ELEMENT_ARRAY_BUFFER (~ *ebo* 0))

  (translation->matrix4f! mat '#f32(-3.0 0.0 -5.0))
  (gl-uniform-matrix4 *render-model-matrix-loc* #f mat)
  (gl-draw-arrays GL_TRIANGLES 0 3)

  (translation->matrix4f! mat '#f32(-1.0 0.0 -5.0))
  (gl-uniform-matrix4 *render-model-matrix-loc* #f mat)
  (gl-draw-elements GL_TRIANGLES #f 3 GL_UNSIGNED_SHORT)

  (translation->matrix4f! mat '#f32(1.0 0.0 -5.0))
  (gl-uniform-matrix4 *render-model-matrix-loc* #f mat)
  (gl-draw-elements-base-vertex GL_TRIANGLES #f 3 GL_UNSIGNED_SHORT 1)

  (translation->matrix4f! mat '#f32(3.0 0.0 -5.0))
  (gl-uniform-matrix4 *render-model-matrix-loc* #f mat)
  (gl-draw-arrays-instanced GL_TRIANGLES 0 3 1)

  (glfw-swap-buffers window)
  )

(define (resize_cb window width height)
  (gl-viewport 0 0 width height)
  (set! *aspect* (/. height width)))

(define (main args)
  (glfw-init)

  (let1 window (glfw-create-window 800 600 "Keypress" #f #f)
    (glfw-set-window-size-callback window resize_cb)
    (glfw-make-context-current window)
    (init)
    (until (glfw-window-should-close window)
      (disp window)
      (glfw-swap-buffers window)
      (glfw-poll-events))

    (glfw-destroy-window window)
    (gl-use-program 0)
    (gl-delete-program *render-prog*)
    (gl-delete-vertex-arrays *vao*)
    (gl-delete-buffers *vbo*))
    (glfw-terminate)
  0)
