;; Example 2-6  Polygon Stipple Patterns

(use gl)
(use gl.glut)

(define *fly*
  '#u8(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 
       #x03 #x80 #x01 #xc0 #x06 #xc0 #x03 #x60
       #x04 #x60 #x06 #x20 #x04 #x30 #x0c #x20
       #x04 #x18 #x18 #x20 #x04 #x0c #x30 #x20
       #x04 #x06 #x60 #x20 #x44 #x03 #xc0 #x22
       #x44 #x01 #x80 #x22 #x44 #x01 #x80 #x22
       #x44 #x01 #x80 #x22 #x44 #x01 #x80 #x22
       #x44 #x01 #x80 #x22 #x44 #x01 #x80 #x22
       #x66 #x01 #x80 #x66 #x33 #x01 #x80 #x22
       #x19 #x81 #x81 #x98 #x0c #xc1 #x83 #x30
       #x07 #xe1 #x87 #xe0 #x03 #x3f #xfc #xc0
       #x03 #x31 #x8c #xc0 #x03 #x33 #xcc #xc0
       #x06 #x64 #x26 #x60 #x0c #xcc #x33 #x30
       #x18 #xcc #x33 #x18 #x10 #xc4 #x23 #x08
       #x10 #x63 #xc6 #x08 #x10 #x30 #x0c #x08
       #x10 #x18 #x18 #x08 #x10 #x00 #x00 #x08))

(define *halftone*
  '#u8(#xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55
       #xaa #xaa #xaa #xaa #x55 #x55 #x55 #x55))

(define (disp)
  (gl-clear GL_COLOR_BUFFER_BIT)
  (gl-color '#f32(1.0 1.0 1.0))
  (gl-enable GL_LINE_STIPPLE)

  (gl-rect '#f32(25.0 25.0) '#f32(125.0 125.0))
  (gl-enable GL_POLYGON_STIPPLE)
  (gl-polygon-stipple *fly*)
  (gl-rect '#f32(125.0 25.0) '#f32(225.0 125.0))
  (gl-polygon-stipple *halftone*)
  (gl-rect '#f32(225.0 25.0) '#f32(325.0 125.0))
  (gl-disable GL_POLYGON_STIPPLE)
  (gl-flush)
  )

(define (init)
  (gl-clear-color 0.0 0.0 0.0 0.0)
  (gl-shade-model GL_FLAT)
  )

(define (reshape w h)
  (gl-viewport 0 0 w h)
  (gl-matrix-mode GL_PROJECTION)
  (gl-load-identity)
  (glu-ortho-2d 0 w 0 h)
  )

(define (keyboard key x y)
  (when (= key 27) (exit 0)))

(define (main args)
  (glut-init args)
  (glut-init-display-mode (logior GLUT_SINGLE GLUT_RGB))
  (glut-init-window-size 350 150)
  (glut-create-window *program-name*)
  (init)
  (glut-display-func disp)
  (glut-reshape-func reshape)
  (glut-keyboard-func keyboard)
  (glut-main-loop)
  0)

  
    

  