;; Example2-9  Enabling and Loading Vertex Arrays

(use gl)
(use gl.glut)

(define *setup-method* 'pointer)
(define *deref-method* 'drawarray)

(define (setup-pointers)
  (gl-enable-client-state GL_VERTEX_ARRAY)
  (gl-enable-client-state GL_COLOR_ARRAY)
  (gl-vertex-pointer 2 '#s32(25 25
                             100 325
                             175 25
                             175 325
                             250 25
                             325 325))
  (gl-color-pointer 3 '#f32(1.0 0.2 0.2
                            0.2 0.2 1.0
                            0.8 1.0 0.2
                            0.75 0.75 0.75
                            0.35 0.35 0.35
                            0.5 0.5 0.5))
  )

(define (setup-interleave)
  (gl-interleaved-arrays GL_C3F_V3F
                         '#f32(1.0 0.2 1.0 100.0 100.0 0.0
                               1.0 0.2 0.2 0.0 200.0 0.0
                               1.0 1.0 0.2 100.0 300.0 0.0
                               0.2 1.0 0.2 200.0 300.0 0.0
                               0.2 1.0 1.0 300.0 200.0 0.0
                               0.2 0.2 1.0 200.0 100.0 0.0))
  )

(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_SMOOTH)
  (setup-pointers))

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (case *deref-method*
    ((drawarray)
     (gl-draw-arrays GL_TRIANGLES 0 6))
    ((arrayelement)
     (gl-begin GL_TRIANGLES)
     (gl-array-element 2)
     (gl-array-element 3)
     (gl-array-element 5)
     (gl-end))
    ((drawelements)
     (gl-draw-elements GL_POLYGON '#u32(0 1 3 4)))
    )
  (gl-flush))

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d 0.0 w 0.0 h))

(define (mouse button state x y)
  (cond
   ((= button GLUT_LEFT_BUTTON)
    (when (= state GLUT_DOWN)
      (case *setup-method*
        ((pointer) (set! *setup-method* 'interleaved)
                   (setup-interleave))
        ((interleaved) (set! *setup-method* 'pointer)
                       (setup-pointers)))
      (glut-post-redisplay)))
   ((or (= button GLUT_MIDDLE_BUTTON)
        (= button GLUT_RIGHT_BUTTON))
    (when (= state GLUT_DOWN)
      (case *deref-method*
        ((drawarray) (set! *deref-method* 'arrayelement))
        ((arrayelement) (set! *deref-method* 'drawelements))
        ((drawelements) (set! *deref-method* 'drawarray)))
      (glut-post-redisplay)))
   ))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size 350 350)
  (glut-init-window-position 100 100)
  (glut-create-window *program-name*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-mouse-func mouse)
  (glut-main-loop)
  0)
