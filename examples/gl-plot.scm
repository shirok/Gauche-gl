;;;
;;; Contributed by Issac Trotts
;;;
;;; $Id: gl-plot.scm,v 1.1 2006-11-09 10:43:55 shirok Exp $
;;;

(use gl)
(use gl.glut)
(use math.const)

(define *pad* 30)

(define (range n)
  (define (aux acc n)
    (if (<= n 0)
      acc
      (aux (cons (- n 1) acc) (- n 1))))
  (aux '() n))

(define (iter f ls)
  (cond
    ((null? ls)) ; do nothing
    ((pair? ls)
     (f (car ls))
     (iter f (cdr ls)))
    (else (error "Cannot iterate over a non-list"))))

(define (iter2 f lsa lsb)
  (cond
    ((or (null? lsa) (null? lsb))) ; do nothing
    ((and (pair? lsa) (pair? lsb))
     (f (car lsa) (car lsb))
     (iter2 f (cdr lsa) (cdr lsb)))
    (else (error "Cannot iterate over a non-list"))))

(define (draw-string x y s)
  (gl-raster-pos x y)
  (iter (lambda (c) (glut-bitmap-character GLUT_BITMAP_8_BY_13
                                           (char->integer c)))
        (string->list s)))

(define plot
  (let1 have-called-glut-init #f
    (lambda (f a b)
      (let ((w 500)
            (h 500))
        (if (not have-called-glut-init)
          (begin
            (glut-init '())
            (set! have-called-glut-init #t)))
        (glut-init-display-mode (logior GLUT_DOUBLE GLUT_RGBA))
        (glut-init-window-size w h)
        (glut-create-window "plot")

                                        ; Initialize GL
        (begin
          (gl-enable GL_BLEND)
          (gl-blend-func GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
          (gl-enable GL_LINE_SMOOTH)
          (gl-clear-color 1.0 1.0 1.0 1.0)
          )
        (let*
            ((xs (map (lambda (x) (+ a (* (- b a) (/ x w)))) (range w)))
             (ys (map f xs))
             (mx (apply max ys))
             (mn (apply min ys)))
          (glut-reshape-func
           (lambda (new-w new-h)
             (gl-viewport 0 0 new-w new-h)
             (set! w new-w)
             (set! h new-h)
             ))
          (glut-display-func
           (lambda ()
             (gl-clear GL_COLOR_BUFFER_BIT)
             (gl-color 0 0 0 1)

             ;; Draw the plot
             (begin
               (gl-viewport *pad* *pad* (- w (* 2 *pad*)) (- h (* 2 *pad*)))
               (gl-matrix-mode GL_PROJECTION)
               (gl-load-identity)
               (glu-ortho-2d a b mn mx)
               (gl-matrix-mode GL_MODELVIEW)
               (gl-load-identity)
               (gl-begin GL_LINE_STRIP)
               (iter2 (lambda (x y) (gl-vertex x y 0.0)) xs ys)
               (gl-end)
               )

             ;; Change to pixel coordinates
             (begin
               (gl-viewport 0 0 w h)
               (gl-matrix-mode GL_PROJECTION)
               (gl-load-identity)
               (glu-ortho-2d 0 w 0 h)
               )

             ;; Show a, b, min, max.
             (let1 pad2 (/ *pad* 2)
               (draw-string pad2        (/ h 2)    (format #f "~a" a))
               (draw-string (- w *pad*) (/ h 2)    (format #f "~a" b))
               (draw-string (/ w 2)     (- h pad2) (format #f "~a" mx))
               (draw-string (/ w 2)     pad2       (format #f "~a" mn))
               )

             ;; Todo: Show axis labels.

             ;; Todo: show tick marks

             (glut-swap-buffers)
             ))
          (glut-keyboard-func
           (lambda (key x y)
             (case (integer->char key)
               ((#\q) (error "Don't worry about this error.")))))
          (guard (exc (else 'foo))
            (glut-main-loop))
          0
          )))))
