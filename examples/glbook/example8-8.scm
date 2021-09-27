;; Example 8-8  Compute an Image's histogram

(use gl)
(use gl.glut)
(use gl.simple.image)
(use gauche.uvector)
(use gauche.sequence)
(use util.match)

(define-constant HISTOGRAM_SIZE 256)

(define *image-file* "../images/flower.rgb")
(define *width* 0)
(define *height* 0)
(define *pixels* #f)

(define (init)
  (gl-pixel-store GL_UNPACK_ALIGNMENT 1)
  (gl-clear-color 0 0 0 0)
  (gl-histogram GL_HISTOGRAM HISTOGRAM_SIZE GL_RGB #f)
  (gl-enable GL_HISTOGRAM)
  )

(define (display-proc)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-raster-pos 1 1)
  (gl-draw-pixels *width* *height* GL_RGB GL_UNSIGNED_BYTE *pixels*)
  (let* ((hist (gl-get-histogram GL_HISTOGRAM #t GL_RGB GL_UNSIGNED_SHORT))
         (peak (fold max 0 hist))
         (factor (/ *height* peak)))
    (gl-begin* GL_LINE_STRIP
      (gl-color 1.0 0.0 0.0)
      (dotimes (i HISTOGRAM_SIZE)
        (gl-vertex i (* (ref hist (* i 3)) factor))))
    (gl-begin* GL_LINE_STRIP
      (gl-color 0.0 1.0 0.0)
      (dotimes (i HISTOGRAM_SIZE)
        (gl-vertex i (* (ref hist (+ (* i 3) 1)) factor))))
    (gl-begin* GL_LINE_STRIP
      (gl-color 0.0 0.0 1.0)
      (dotimes (i HISTOGRAM_SIZE)
        (gl-vertex i (* (ref hist (+ (* i 3) 2)) factor))))
    )
  (gl-flush))

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d 0.0 w 0.0 h)
  (gl-matrix-mode GL_MODELVIEW)
  (gl-load-identity)
  )

(define (keyboard key x y)
  (cond
   ((= key 27)                          ;ESC
    (exit 0)))
  )

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))

  (unless (file-exists? *image-file*)
    (print "Couldn't find image file: " *image-file*)
    (exit 0))

  (match-let1 (width height depth image)
      (read-sgi-image *image-file*)
    (set! *width* width)
    (set! *height* height)
    (set! *pixels* image)

    (glut-init-window-size width height)
    (glut-init-window-position 100 100)
    (glut-create-window (car args))

    (unless (gl-extension-available? 'GL_ARB_imaging)
      (print "GL_ARB_imaging is not supported on this platform")
      (exit 0))

    (init)
    (glut-reshape-func reshape)
    (glut-keyboard-func keyboard)
    (glut-display-func display-proc)
    (glut-main-loop)
    0))
