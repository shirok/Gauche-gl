;; Example 10-5  Depth-of-field effect

(use gl)
(use gl.glut)
(use gauche.uvector)
(use math.const)

(define *teapot-list* #f)

(define (acc-frustum left right bottom top near far
                     pixdx pixdy eyedx eyedy focus)
  (let ((viewport (gl-get-integer GL_VIEWPORT))
        (xwsize   (- right left))
        (ywsize   (- top bottom)))
    (let ((dx (- (+ (* pixdx (/ xwsize (ref viewport 2)))
                    (* eyedx (/ near focus)))))
          (dy (- (+ (* pixdy (/ ywsize (ref viewport 3)))
                    (* eyedy (/ near focus))))))
      (gl-matrix-mode GL_PROJECTION)
      (gl-load-identity)
      (gl-frustum (+ left dx) (+ right dx) (+ bottom dy) (+ top dy) near far)
      (gl-matrix-mode GL_MODELVIEW)
      (gl-load-identity)
      (gl-translate (- eyedx) (- eyedy) 0.0)
      )))

(define (acc-perspective fovy aspect near far pixdx pixdy eyedx eyedy focus)
  (let* ((fov2   (/ (* fovy pi/180) 2.0))
         (top    (/ near (/ (cos fov2) (sin fov2))))
         (bottom (- top))
         (right  (* top aspect))
         (left   (- right)))
    (acc-frustum left right bottom top near far pixdy pixdy eyedx eyedy focus)
    ))

(define (init)
  (gl-light GL_LIGHT0 GL_AMBIENT '#f32(0.0 0.0 0.0 1.0))
  (gl-light GL_LIGHT0 GL_DIFFUSE '#f32(1.0 1.0 1.0 1.0))
  (gl-light GL_LIGHT0 GL_POSITION '#f32(0.0 3.0 3.0 0.0))
  (gl-light-model GL_LIGHT_MODEL_AMBIENT '#f32(0.2 0.2 0.2 1.0))
  (gl-light-model GL_LIGHT_MODEL_LOCAL_VIEWER '#f32(0.0))

  (gl-front-face GL_CW)
  (gl-enable GL_LIGHTING)
  (gl-enable GL_LIGHT0)
  (gl-enable GL_AUTO_NORMAL)
  (gl-enable GL_NORMALIZE)
  (gl-enable GL_DEPTH_TEST)

  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-clear-accum 0.0 0.0 0.0 0.0)

  (set! *teapot-list* (gl-gen-lists 1))
  (gl-new-list *teapot-list* GL_COMPILE)
  (glut-solid-teapot 0.5)
  (gl-end-list)
  )

(define (render-teapot x y z
                       ambr ambg ambb
                       difr difg difb
                       specr specg specb shine)
  (gl-push-matrix*
   (gl-translate x y z)
   (gl-material GL_FRONT GL_AMBIENT  (f32vector ambr ambg ambb 1.0))
   (gl-material GL_FRONT GL_DIFFUSE  (f32vector difr difg difb 1.0))
   (gl-material GL_FRONT GL_SPECULAR (f32vector specr specg specb 1.0))
   (gl-material GL_FRONT GL_SHININESS (* shine 128.0))
   (gl-call-list *teapot-list*)
   ))

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
    (dotimes (jitter 8)
      (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
      (acc-perspective 45.0 (/ (ref viewport 2) (ref viewport 3))
                       1.0 15.0 0.0 0.0
                       (* 0.33 (car (ref J8 jitter)))
                       (* 0.33 (cadr (ref J8 jitter)))
                       5.0)
      (render-teapot -1.1 -0.5 -4.5 0.1745 0.01175
                     0.01175 0.61424 0.04136 0.04136
                     0.727811 0.626959 0.626959 0.6)
      (render-teapot -0.5 -0.5 -5.0 0.24725 0.1995
                     0.0745 0.75164 0.60648 0.22648
                     0.628281 0.555802 0.366065 0.4)
      (render-teapot 0.2 -0.5 -5.5 0.19225 0.19225
                     0.19225 0.50754 0.50754 0.50754
                     0.508273 0.508273 0.508273 0.4)
      (render-teapot 1.0 -0.5 -6.0 0.0215 0.1745 0.0215
                     0.07568 0.61424 0.07568 0.633
                     0.727811 0.633 0.6)
      (render-teapot 1.8 -0.5 -6.5 0.0 0.1 0.06 0.0
                     0.50980392 0.50980392 0.50196078
                     0.50196078 0.50196078 .25)
      (gl-accum GL_ACCUM (/ 8)))
    (gl-accum GL_RETURN 1.0)
    (gl-flush)
    ))

(define (reshape-proc w h)
  (gl-viewport 0 0 w h)
  )

(define (keyboard-proc key x y)
  (when (= key 27) (exit 0)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH GLUT_ACCUM))
  (glut-init-window-size 400 400)
  (glut-init-window-position 100 100)
  (glut-create-window (car args))
  (init)
  (glut-reshape-func reshape-proc)
  (glut-display-func display-proc)
  (glut-keyboard-func keyboard-proc)
  (glut-main-loop)
  0)
