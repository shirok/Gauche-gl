;;;
;;; grid.scm - Create a grid
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: grid.scm,v 1.1 2001-10-15 08:21:04 shirok Exp $
;;;

(define-module gl.grid
  (use gl)
  (export ggl-make-grid)
  )
(select-module gl.grid)

(define (ggl-make-grid xsize ysize)
  (let* ((2xsize+1 (+ (* 2 xsize) 1))
         (2ysize+1 (+ (* 2 ysize) 1))
         (nvtx   (* 2xsize+1 2ysize+1))
         (nlines (+ 2xsize+1 2ysize+1))
         (vtx    (make-f32vector (* nvtx 3)))
         (lines  (make-u32vector (* nlines 2))))
    (dotimes (nx 2xsize+1)
      (dotimes (ny 2ysize+1)
        (let ((n (+ (* nx 2ysize+1) ny)))
          (set! (f32vector-ref vtx (* n 3))       (- nx xsize))
          (set! (f32vector-ref vtx (+ (* n 3) 1)) (- ny ysize))
          (set! (f32vector-ref vtx (+ (* n 3) 2)) 0))))
    (dotimes (nx 2xsize+1)
      (set! (u32vector-ref lines (* nx 2))       (* nx 2xsize+1))
      (set! (u32vector-ref lines (+ (* nx 2) 1)) (+ (* nx 2xsize+1) (* 2 xsize))))
    (dotimes (ny 2ysize+1)
      (set! (u32vector-ref lines (+ (* 2 2xsize+1) (* ny 2))) ny)
      (set! (u32vector-ref lines (+ (* 2 2xsize+1) (* ny 2) 1)) (+ ny (* 2xsize+1 ysize 2))))
    (lambda ()
      (gl-vertex-pointer 3 vtx)
      (gl-draw-elements |GL_LINES| lines))
    ))

(provide "gl/grid")
