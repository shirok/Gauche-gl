;;
;; Calculate and show Mandelbrot set.
;;

(use srfi-4)
(use gl)
(use gl.glut)

(define *size*  256)
(define *image* (make-u8vector (* *size* *size* 3) 0))
(define *tex* #f)

(define (fill-image)
  (dotimes (y *size*)
    (dotimes (x *size*)
      (let ((i (* (+ (* y *size*) x) 3))
            (z (make-rectangular (- (* 3 (/ x *size*)) 2)
                                 (- (* 3 (/ y *size*)) 1.5))))
        (letrec ((rank (lambda (zn n)
                         (cond ((>= n 16) 0)
                               ((>= (magnitude zn) 2) n)
                               (else (rank (+ (* zn zn) z) (+ n 1)))))))
          (let ((r (rank z 0)))
            (u8vector-set! *image* i       (ash (logand r #xc) 4))
            (u8vector-set! *image* (+ i 1) (ash (logand r #x2) 6))
            (u8vector-set! *image* (+ i 2) (ash (logand r #x1) 7))
            ))))))

(define (init)
  (fill-image)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)
  (set! *tex* (u32vector-ref (gl-gen-textures 1) 0))
  (gl-bind-texture GL_TEXTURE_2D *tex*)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (gl-tex-image-2d GL_TEXTURE_2D 0 GL_RGB *size* *size* 0
                   GL_RGB GL_UNSIGNED_BYTE *image*)
  )

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-enable GL_TEXTURE_2D)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_REPLACE)
  (gl-bind-texture GL_TEXTURE_2D *tex*)
  (gl-begin GL_QUADS)
  (gl-tex-coord '#f32(0.0 0.0)) (gl-vertex '#f32(0.0 0.0))
  (gl-tex-coord '#f32(0.0 1.0)) (gl-vertex '#f32(0.0 1.0))
  (gl-tex-coord '#f32(1.0 1.0)) (gl-vertex '#f32(1.0 1.0))
  (gl-tex-coord '#f32(1.0 0.0)) (gl-vertex '#f32(1.0 0.0))
  (gl-end)
  (gl-flush)
  (gl-disable GL_TEXTURE_2D)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d 0 1 0 1)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  )

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 256 256)
  (glut-create-window "mandelbrot")
  (init)
  (glut-reshape-func reshape)
  (glut-display-func disp)
  (glut-main-loop)
  0
  )
