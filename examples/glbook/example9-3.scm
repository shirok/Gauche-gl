;; Example 9-3  Replacing a Texture Subimage

(use gl)
(use gl.glut)
(use gauche.uvector)

;; Create checkerboard image
(define-constant *check-image-width* 64)
(define-constant *check-image-height* 64)
(define *check-image*
  (make-u8vector (* *check-image-height* *check-image-width* 4)))
(define-constant *sub-image-width* 16)
(define-constant *sub-image-height* 16)
(define *sub-image*
  (make-u8vector (* *sub-image-height* *sub-image-width* 4)))

(define *texname* 0)

(define (make-check-images)
  ;; NB: this must be easier once uniform array is implemented.
  (dotimes (i *check-image-height*)
    (dotimes (j *check-image-width*)
      (let ((ij (* (+ (* i *check-image-width*) j) 4))
            (c  (if (or (and (zero? (logand i #x08))
                             (zero? (logand j #x08)))
                        (and (not (zero? (logand i #x08)))
                             (not (zero? (logand j #x08)))))
                    0
                    255)))
        (set! (ref *check-image* ij) c)
        (set! (ref *check-image* (+ ij 1)) c)
        (set! (ref *check-image* (+ ij 2)) c)
        (set! (ref *check-image* (+ ij 3)) 255))))
  (dotimes (i *sub-image-height*)
    (dotimes (j *sub-image-width*)
      (let ((ij (* (+ (* i *sub-image-width*) j) 4))
            (c  (if (or (and (zero? (logand i #x04))
                             (zero? (logand j #x04)))
                        (and (not (zero? (logand i #x04)))
                             (not (zero? (logand j #x04)))))
                    0
                    255)))
        (set! (ref *sub-image* ij) c)
        (set! (ref *sub-image* (+ ij 1)) 0)
        (set! (ref *sub-image* (+ ij 2)) 0)
        (set! (ref *sub-image* (+ ij 3)) 255))))
  )

(define (init)
  (gl-clear-color 0 0 0 0)
  (gl-shade-model GL_FLAT)
  (gl-enable GL_DEPTH_TEST)
  (make-check-images)
  (gl-pixel-store GL_UNPACK_ALIGNMENT 1)

  (let1 texnames (gl-gen-textures 1)
    (set! *texname* (ref texnames 0))
    (gl-bind-texture GL_TEXTURE_2D *texname*))
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
  (gl-tex-parameter GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
  (gl-tex-image-2d GL_TEXTURE_2D 0 GL_RGBA
                   *check-image-width* *check-image-height*
                   0 GL_RGBA GL_UNSIGNED_BYTE *check-image*)
  )

(define (display)
  (gl-clear (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
  (gl-enable GL_TEXTURE_2D)
  (gl-tex-env GL_TEXTURE_ENV GL_TEXTURE_ENV_MODE GL_DECAL)
  (gl-bind-texture GL_TEXTURE_2D *texname*)

  (gl-begin GL_QUADS)
  (gl-tex-coord 0.0 0.0) (gl-vertex -2.0 -1.0 0.0)
  (gl-tex-coord 0.0 1.0) (gl-vertex -2.0 1.0 0.0)
  (gl-tex-coord 1.0 1.0) (gl-vertex 0.0 1.0 0.0)
  (gl-tex-coord 1.0 0.0) (gl-vertex 0.0 -1.0 0.0)

  (gl-tex-coord 0.0 0.0) (gl-vertex 1.0 -1.0 0.0)
  (gl-tex-coord 0.0 1.0) (gl-vertex 1.0 1.0 0.0)
  (gl-tex-coord 1.0 1.0) (gl-vertex 2.41421 1.0 -1.41421)
  (gl-tex-coord 1.0 0.0) (gl-vertex 2.41421 -1.0 -1.41421)
  (gl-end)
  (gl-flush)
  (gl-disable GL_TEXTURE_2D)
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
   ((or (= key (char->integer #\s))
        (= key (char->integer #\S)))
    (gl-bind-texture GL_TEXTURE_2D *texname*)
    (gl-tex-sub-image-2d GL_TEXTURE_2D 0 12 44
                         *sub-image-width* *sub-image-height*
                         GL_RGBA GL_UNSIGNED_BYTE *sub-image*)
    (glut-post-redisplay))
   ((or (= key (char->integer #\r))
        (= key (char->integer #\R)))
    (gl-bind-texture GL_TEXTURE_2D *texname*)
    (gl-tex-image-2d GL_TEXTURE_2D 0 GL_RGBA
                     *check-image-width* *check-image-height*
                     0 GL_RGBA GL_UNSIGNED_BYTE *check-image*)
    (glut-post-redisplay))
   ((= key 27) (exit 0))
   ))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB GLUT_DEPTH))
  (glut-init-window-size 250 250)
  (glut-init-window-position 100 100)
  (glut-create-window (car args))
  (init)
  (glut-display-func display)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)
