;; Example 9-7  Binding Texture Objects

(use gl)
(use gl.glut)
(use gauche.uvector)

;; Create checkerboard image
(define-constant *check-image-width* 64)
(define-constant *check-image-height* 64)
(define *check-image*
  (make-u8vector (* *check-image-height* *check-image-width* 4)))
(define *other-image*
  (make-u8vector (* *check-image-height* *check-image-width* 4)))

(define *texname* #f)

(define (make-check-images)
  (dotimes (i *check-image-height*)
    (dotimes (j *check-image-width*)
      (let ((ij (* (+ (* i *check-image-width*) j) 4))
            (c  (if (or (and (zero? (logand i #x08))
                             (zero? (logand j #x08)))
                        (and (not (zero? (logand i #x08)))
                             (not (zero? (logand j #x08)))))
                    0
                    255))
            (cc (if (or (and (zero? (logand i #x10))
                             (zero? (logand j #x10)))
                        (and (not (zero? (logand i #x10)))
                             (not (zero? (logand j #x10)))))
                    0
                    255))
            )
        (set! (ref *check-image* ij) c)
        (set! (ref *check-image* (+ ij 1)) c)
        (set! (ref *check-image* (+ ij 2)) c)
        (set! (ref *check-image* (+ ij 3)) 255)
        (set! (ref *other-image* ij) cc)
        (set! (ref *other-image* (+ ij 1)) 0)
        (set! (ref *other-image* (+ ij 2)) 0)
        (set! (ref *other-image* (+ ij 3)) 255))))
  )

(define (init)
  (gl-clear-color 0 0 0 0)
  (gl-shade-model GL_FLAT)
  (gl-enable GL_DEPTH_TEST)

  (make-check-images)
  (gl-pixel-store GL_UNPACK_ALIGNMENT 1)

  (set! *texname* (gl-gen-textures 2))
  (gl-bind-texture GL_TEXTURE_2D (ref *texname* 0))
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (gl-tex-image-2d GL_TEXTURE_2D 0 GL_RGBA
                   *check-image-width* *check-image-height*
                   0 GL_RGBA GL_UNSIGNED_BYTE *check-image*)

  (gl-bind-texture GL_TEXTURE_2D (ref *texname* 1))
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  (gl-tex-image-2d GL_TEXTURE_2D 0 GL_RGBA
                   *check-image-width* *check-image-height*
                   0 GL_RGBA GL_UNSIGNED_BYTE *other-image*)
  (gl-enable GL_TEXTURE_2D)
  )

(define (disp)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

  (gl-bind-texture GL_TEXTURE_2D (ref *texname* 0))
  (gl-begin GL_QUADS)
  (gl-tex-coord 0.0 0.0) (gl-vertex -2.0 -1.0 0.0)
  (gl-tex-coord 0.0 1.0) (gl-vertex -2.0 1.0 0.0)
  (gl-tex-coord 1.0 1.0) (gl-vertex 0.0 1.0 0.0)
  (gl-tex-coord 1.0 0.0) (gl-vertex 0.0 -1.0 0.0)
  (gl-end)

  (gl-bind-texture GL_TEXTURE_2D (ref *texname* 1))
  (gl-begin GL_QUADS)
  (gl-tex-coord 0.0 0.0) (gl-vertex 1.0 -1.0 0.0)
  (gl-tex-coord 0.0 1.0) (gl-vertex 1.0 1.0 0.0)
  (gl-tex-coord 1.0 1.0) (gl-vertex 2.41421 1.0 -1.41421)
  (gl-tex-coord 1.0 0.0) (gl-vertex 2.41421 -1.0 -1.41421)
  (gl-end)
  (gl-flush)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-perspective 60.0 (/ w h) 1.0 30.0)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  (gl-translate 0.0 0.0 -3.6))

(define (keyboard key x y)
  (cond
   ((= key 27) (exit 0))
   ))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size 250 250)
  (glut-init-window-position 100 100)
  (glut-create-window (car args))
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)
