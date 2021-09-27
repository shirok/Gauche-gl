;; Example 8-6  Using 2D Convolution Filters

(use gl)
(use gl.glut)
(use gl.simple.image)
(use gauche.uvector)
(use util.match)

;; convolution filters
(define *horizontal*
  '#f32( 0.0 -1.0  0.0
         0.0  1.0  0.0
         0.0  0.0  0.0))

(define *vertical*
  '#f32( 0.0  0.0  0.0
        -1.0  1.0  0.0
         0.0  0.0  0.0))

(define *laplacian*
  '#f32(-0.125 -0.125 -0.125
        -0.125  1.0   -0.125
        -0.125 -0.125 -0.125))

(define *image-file* "../images/noturn.rgb")
(define *width* #f)
(define *height* #f)
(define *image* #f)

(define (init)
  (gl-pixel-store GL_UNPACK_ALIGNMENT 1)
  (gl-clear-color 0 0 0 0)
  (gl-convolution-filter-2d GL_CONVOLUTION_2D GL_LUMINANCE 3 3
                            GL_LUMINANCE GL_FLOAT *horizontal*)
  (gl-enable GL_CONVOLUTION_2D)
  )

(define (display-proc)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-raster-pos 1 1)
  (gl-draw-pixels *width* *height* GL_RGB GL_UNSIGNED_BYTE *image*)
  (gl-flush)
  )

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
   ((= key (char->integer #\h))
    (print "Using horizontal filter")
    (gl-convolution-filter-2d GL_CONVOLUTION_2D GL_LUMINANCE 3 3
                              GL_LUMINANCE GL_FLOAT *horizontal*)
    (gl-enable GL_CONVOLUTION_2D)
    (glut-post-redisplay)
    )
   ((= key (char->integer #\v))
    (print "Using vertical filter")
    (gl-convolution-filter-2d GL_CONVOLUTION_2D GL_LUMINANCE 3 3
                              GL_LUMINANCE GL_FLOAT *vertical*)
    (gl-enable GL_CONVOLUTION_2D)
    (glut-post-redisplay)
    )
   ((= key (char->integer #\l))
    (print "Using laplacian filter")
    (gl-convolution-filter-2d GL_CONVOLUTION_2D GL_LUMINANCE 3 3
                              GL_LUMINANCE GL_FLOAT *laplacian*)
    (gl-enable GL_CONVOLUTION_2D)
    (glut-post-redisplay)
    )
   ((= key (char->integer #\n))
    (print "No filter")
    (gl-disable GL_CONVOLUTION_2D)
    (glut-post-redisplay))
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
    (set! *image* image)

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
