;; Example 10-2  Antialiasing by jittering the orthographic projection

(use gl)
(use gl.glut)
(use math.const)

(define (init)
  (let ((mat-ambient    '#f32(1.0 1.0 1.0 1.0))
        (mat-specular   '#f32(1.0 1.0 1.0 1.0))
        (light-position '#f32(0.0 0.0 10.0 1.0))
        (lm-ambient     '#f32(0.2 0.2 0.2 1.0)))
    (gl-material GL_FRONT GL_AMBIENT mat-ambient)
    (gl-material GL_FRONT GL_SPECULAR mat-specular)
    (gl-material GL_FRONT GL_SHININESS 50.0)
    (gl-light GL_LIGHT0 GL_POSITION light-position)
    (gl-light-model GL_LIGHT_MODEL_AMBIENT lm-ambient)
    
    (gl-enable GL_LIGHT0)
    (gl-enable GL_LIGHTING)
    (gl-enable GL_DEPTH_TEST)
    (gl-shade-model GL_FLAT)

    (gl-clear-color 0.0 0.0 0.0 0.0)
    (gl-clear-accum 0.0 0.0 0.0 0.0)
    ))

(define (display-objects)
  (let ((torus-diffuse  '#f32(0.7 0.7 0.0 1.0))
        (cube-diffuse   '#f32(0.0 0.7 0.7 1.0))
        (sphere-diffuse '#f32(0.7 0.0 0.7 1.0))
        (octa-diffuse   '#f32(0.7 0.4 0.4 1.0)))

    (gl-push-matrix*
     (gl-translate 0 0 -5.0)
     (gl-rotate 30.0 1.0 0.0 0.0)

     (gl-push-matrix*
      (gl-translate -0.8 0.35 0)
      (gl-rotate 100.0 1.0 0.0 0.0)
      (gl-material GL_FRONT GL_DIFFUSE torus-diffuse)
      (glut-solid-torus 0.275 0.85 16 16))

     (gl-push-matrix*
      (gl-translate -0.75 -0.5 0)
      (gl-rotate 45.0 0.0 0.0 1.0)
      (gl-rotate 54.0 1.0 0.0 0.0)
      (gl-material GL_FRONT GL_DIFFUSE cube-diffuse)
      (glut-solid-cube 1.5))

     (gl-push-matrix*
      (gl-translate 0.75 0.6 0)
      (gl-rotate 30.0 1.0 0.0 0.0)
      (gl-material GL_FRONT GL_DIFFUSE sphere-diffuse)
      (glut-solid-sphere 1.0 16 16))

     (gl-push-matrix*
      (gl-translate 0.7 -0.9 0.25)
      (gl-material GL_FRONT GL_DIFFUSE octa-diffuse)
      (glut-solid-octahedron))
     )
    ))

(define-constant ACSIZE 8)

(define-constant J8            ;; jitter offsets
  '#((-0.334818  0.435331)
     ( 0.286438 -0.393495)
     ( 0.459462  0.141540)
     (-0.414498 -0.192829)
     (-0.183790  0.082102)
     (-0.079263 -0.317383)
     ( 0.102254  0.299133)
     ( 0.164216 -0.054399)))

(define (display-proc)
  (let1 viewport (gl-get-integer GL_VIEWPORT)
    (gl-clear GL_ACCUM_BUFFER_BIT)
    (dotimes (jitter ACSIZE)
      (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (gl-push-matrix*
       ;; Note that 4.5 is the distance in world space between
       ;; left and right and bottom and top.
       ;; This formula converts fractional pixel movement to 
       ;; world coordinates.
       (gl-translate (/ (* (car (ref J8 jitter))  4.5) (ref viewport 2))
                     (/ (* (cadr (ref J8 jitter)) 4.5) (ref viewport 3))
                     0.0)
       (display-objects))
      (gl-accum GL_ACCUM (/ ACSIZE)))
    (gl-accum GL_RETURN 1.0)
    (gl-flush)
    ))

(define (reshape-proc w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (if (<= w h)
    (gl-ortho -2.25 2.25 (* -2.25 (/ h w)) (* 2.25 (/ h w)) -10.0 10.0)
    (gl-ortho (* -2.25 (/ w h)) (* 2.25 (/ w h)) -2.25 2.25 -10.0 10.0))
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  )

(define (keyboard-proc key x y)
  (when (= key 27) (exit 0)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH GLUT_ACCUM))
  (glut-init-window-size 250 250)
  (glut-init-window-position 100 100)
  (glut-create-window (car args))
  (init)
  (glut-reshape-func reshape-proc)
  (glut-display-func display-proc)
  (glut-keyboard-func keyboard-proc)
  (glut-main-loop)
  0)

                      

