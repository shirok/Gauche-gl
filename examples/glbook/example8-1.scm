;; Example 8-1 Drawing a Bitmapped Character

(use gl)
(use gl.glut)
(use gauche.uvector)

(define *rasters*
  '#u8(#xc0 #x00 #xc0 #x00 #xc0 #x00 #xc0 #x00 #xc0 #x00
       #xff #x00 #xff #x00 #xc0 #x00 #xc0 #x00 #xc0 #x00
       #xff #xc0 #xff #xc0))

(define (init)
  (gl-pixel-store GL_UNPACK_ALIGNMENT 1)
  (gl-clear-color 0.0 0.0 0.0 0.0))

(define (display)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color 1.0 1.0 1.0)
  (gl-raster-pos 20 20)
  (gl-bitmap 10 12 0.0 0.0 11.0 0.0 *rasters*)
  (gl-bitmap 10 12 0.0 0.0 11.0 0.0 *rasters*)
  (gl-bitmap 10 12 0.0 0.0 11.0 0.0 *rasters*)
  (gl-flush)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (gl-ortho 0.0 w 0.0 h -1.0 1.0)
  (gl-matrix-mode GL_MODELVIEW)
  )

(define (keyboard key x y)
  (cond
   ((= key 27)                          ;ESC
    (exit 0)))
  )

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 100 100)
  (glut-create-window (car args))
  (init)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-display-func display)
  (glut-main-loop)
  0)
