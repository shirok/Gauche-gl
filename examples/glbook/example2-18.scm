;; Example 2-18 Using vertex array object

(use gl)
(use gl.glut)
(use gauche.record)
(use gauche.uvector)
(use math.const)

(define-record-type xform #t #t
  xlate                                 ;Translation
  angle
  axis)

(define-constant Cube 0)
(define-constant Cone 1)
(define-constant NumVAOs 2)

(define *vao* #f) ; set by init
(define *prim-types* (make-vector NumVAOs))
(define *num-elements* (make-vector NumVAOs))
(define *xform*
  `#(,(make-xform #f32(-2.0  0.0  0.0)  0.0  #f32( 0.0  1.0  0.0))
     ,(make-xform #f32( 0.0  0.0  2.0)  0.0  #f32( 1.0  0.0  0.0))))
(define *angle* 0.0)

(define (init)
  (define Vertices 0)
  (define Colors 1)
  (define Elements 2)
  (define NumVBOs 3)

  (define (initCube)
    (define cube-verts
      '#f32(-1.0 -1.0 -1.0
            -1.0 -1.0  1.0
            -1.0  1.0 -1.0
            -1.0  1.0  1.0
             1.0 -1.0 -1.0
             1.0 -1.0  1.0
             1.0  1.0 -1.0
             1.0  1.0  1.0))
    (define cube-colors
      '#f32( 0.0  0.0  0.0
             0.0  0.0  1.0
             0.0  1.0  0.0
             0.0  1.0  1.0
             1.0  0.0  0.0
             1.0  0.0  1.0
             1.0  1.0  0.0
             1.0  1.0  1.0))
    (define cube-indices
      '#u8(0 1 3
           0 3 2
           4 6 7
           4 7 5
           2 3 7
           2 7 6
           0 4 5
           0 5 1
           0 2 6
           0 6 4
           1 5 7
           1 7 3))
    (define buffers)

    (gl-bind-vertex-array (~ *vao* Cube))
    (set! buffers (gl-gen-buffers NumVBOs))
    (gl-bind-buffer GL_ARRAY_BUFFER (~ buffers Vertices))
    (gl-buffer-data GL_ARRAY_BUFFER 0 cube-verts GL_STATIC_DRAW)
    (gl-vertex-pointer 3 #f 0 0 GL_FLOAT)
    (gl-enable-client-state GL_VERTEX_ARRAY)

    (gl-bind-buffer GL_ARRAY_BUFFER (~ buffers Colors))
    (gl-buffer-data GL_ARRAY_BUFFER 0 cube-colors GL_STATIC_DRAW)
    (gl-color-pointer 3 #f 0 0 GL_FLOAT)
    (gl-enable-client-state GL_COLOR_ARRAY)

    (gl-bind-buffer GL_ELEMENT_ARRAY_BUFFER (~ buffers Elements))
    (gl-buffer-data GL_ELEMENT_ARRAY_BUFFER 0 cube-indices GL_STATIC_DRAW)

    (set! (~ *prim-types* Cube) GL_TRIANGLES)
    (set! (~ *num-elements* Cube) (uvector-length cube-indices))
    )
  (define (f32set3! v k x y z)
    (f32vector-set! v (* k 3) x)
    (f32vector-set! v (+ (* k 3) 1) y)
    (f32vector-set! v (+ (* k 3) 2) z))
  (define (initCone)
    (define numConePoints 36)
    (define cone-verts  (make-f32vector (* (+ numConePoints 1) 3)))
    (define cone-colors (make-f32vector (* (+ numConePoints 1) 3)))
    (define cone-indices (make-u8vector 37))
    (define dTheta (/ (* 2 pi) (- numConePoints 1)))
    (define buffers)

    (f32set3! cone-verts  0 0.0 0.0 1.0)
    (f32set3! cone-colors 0 1.0 1.0 1.0)
    (do ([i 0 (+ i 1)]
         [j 1 (+ j 1)])
        [(= i numConePoints)]
      (let1 theta (* i dTheta)
        (f32set3! cone-verts j (cos theta) (sin theta) 0.0)
        (f32set3! cone-colors j (cos theta) (sin theta) 0.0)
        (u8vector-set! cone-indices j j)))

    (gl-bind-vertex-array (~ *vao* Cone))
    (set! buffers (gl-gen-buffers NumVBOs))
    (gl-bind-buffer GL_ARRAY_BUFFER (~ buffers Vertices))
    (gl-buffer-data GL_ARRAY_BUFFER 0 cone-verts GL_STATIC_DRAW)
    (gl-vertex-pointer 3 #f 0 0 GL_FLOAT)
    (gl-enable-client-state GL_VERTEX_ARRAY)

    (gl-bind-buffer GL_ARRAY_BUFFER (~ buffers Colors))
    (gl-buffer-data GL_ARRAY_BUFFER 0 cone-colors GL_STATIC_DRAW)
    (gl-color-pointer 3 #f 0 0 GL_FLOAT)
    (gl-enable-client-state GL_COLOR_ARRAY)

    (gl-bind-buffer GL_ELEMENT_ARRAY_BUFFER (~ buffers Elements))
    (gl-buffer-data GL_ELEMENT_ARRAY_BUFFER 0 cone-indices GL_STATIC_DRAW)

    (set! (~ *prim-types* Cone) GL_TRIANGLE_FAN)
    (set! (~ *num-elements* Cone) (uvector-length cone-indices))
    )

  (set! *vao* (gl-gen-vertex-arrays NumVAOs))
  (initCube)
  (initCone)
  (gl-enable GL_DEPTH_TEST)
  )

(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-push-matrix)
  (gl-rotate *angle* 0.0 1.0 0.0)

  (dotimes [i NumVAOs]
    (gl-push-matrix)
    (gl-translate (~ *xform* i 'xlate 0)
                  (~ *xform* i 'xlate 1)
                  (~ *xform* i 'xlate 2))
    (gl-rotate (~ *xform* i 'angle)
               (~ *xform* i 'axis 0)
               (~ *xform* i 'axis 1)
               (~ *xform* i 'axis 2))
    (gl-bind-vertex-array (~ *vao* i))
    (gl-draw-elements (~ *prim-types* i) (~ *num-elements* i) GL_UNSIGNED_BYTE)
    (let1 e (gl-get-error)
      (unless (zero? e) (print e)))
    (gl-pop-matrix))

  (gl-pop-matrix)
  (glut-swap-buffers))

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
  (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGB GLUT_DEPTH))
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
