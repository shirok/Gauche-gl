;;
;; test for gl.math3d module
;;

(use gauche.test)
(add-load-path "../lib")

(test-start "gl.math3d")
(use gl.math3d)
(use gauche.sequence)
(use math.const)

(define (nearly=? a b)
  (let ((sizea (size-of a))
        (sizeb (size-of b)))
    (and (= sizea sizeb)
         (let loop ((i 0))
           (cond ((>= i sizea) #t)
                 ((< (abs (- (ref a i) (ref b i))) 1.0e-4) (loop (+ i 1)))
                 (else #f))))))

;;------------------------------------------------------------------
(test-section "vector4f")

(test "vector4f" #t
      (lambda () (vector4f? (vector4f 0 1 2 3))))
(test "vector4f reader" #t
      (lambda () (equal? (vector4f 0 1 2 3) #,(vector4f 0 1 2 3))))
(test "vector4f converters" #t
      (lambda ()
        (let1 v (vector4f 0 1 2 3)
          (and (equal? v (list->vector4f '(0 1 2 3)))
               (equal? v (f32vector->vector4f '#f32(0.0 1.0 2.0 3.0)))
               (equal? v (list->vector4f (vector4f->list v)))
               (equal? v (f32vector->vector4f (vector4f->f32vector v)))
               (equal? v (coerce-to <vector4f> '(0 1 2 3)))
               (equal? v (coerce-to <vector4f> '#(0 1 2 3)))
               (equal? v (coerce-to <vector4f> '#f32(0 1 2 3)))
               ))))
(test "vector4f ref" '(0.0 1.0 2.0 3.0)
      (lambda ()
        (let1 v (vector4f 0 1 2 3)
          (map (pa$ vector4f-ref v) '(0 1 2 3)))))
(test "vector4f set" #,(vector4f 3.0 2.0 1.0 0.0)
      (lambda ()
        (let1 v (vector4f 0 0 0 0)
          (set! (vector4f-ref v 0) 3.0)
          (set! (vector4f-ref v 1) 2.0)
          (set! (vector4f-ref v 2) 1.0)
          (set! (vector4f-ref v 3) 0.0)
          v)))
(test "vector4f +" #,(vector4f 3.0 5.0 7.0 9.0)
      (lambda ()
        (+ #,(vector4f 1.0 2.0 3.0 4.0)
           #,(vector4f 2.0 3.0 4.0 5.0))))
(test "vector4f +" #,(vector4f 1.0 1.0 1.0 1.0)
      (lambda ()
        (+ #,(vector4f 1.0 2.0 3.0 4.0)
           #,(vector4f 2.0 3.0 4.0 5.0)
           #,(vector4f -2.0 -4.0 -6.0 -8.0))))
(test "vector4f -" #,(vector4f -1.0 -2.0 -3.0 -4.0)
      (lambda ()
        (- #,(vector4f 1.0 2.0 3.0 4.0)
           #,(vector4f 2.0 4.0 6.0 8.0))))
(test "vector4f dot" 40.0
      (lambda ()
        (vector4f-dot #,(vector4f 1.0 2.0 3.0 4.0)
                      #,(vector4f 2.0 3.0 4.0 5.0))))
(test "vector4f cross" #,(vector4f -4.0 8.0 -4.0 0.0)
      (lambda ()
        (vector4f-cross #,(vector4f 1.0 2.0 3.0 0.0)
                        #,(vector4f 5.0 6.0 7.0 0.0))))
(test "vector4f normalize" #,(vector4f 0.5 0.5 0.5 0.5)
      (lambda () (vector4f-normalize (vector4f 1 1 1 1))))
(test "vector4f normalize!" #,(vector4f 0.5 0.5 0.5 0.5)
      (lambda ()
        (let1 v (vector4f 4 4 4 4)
          (vector4f-normalize! v)
          v)))

;;------------------------------------------------------------------
(test-section "point4f")

(test "point4f" #t
      (lambda () (point4f? (point4f 1 2 3))))
(test "point4f reader" #t
      (lambda () (equal? (point4f 1 2 3) #,(point4f 1 2 3))))
(test "point4f converters" #t
      (lambda ()
        (let1 v (point4f 1 2 3)
          (and (equal? v (list->point4f '(1 2 3)))
               (equal? v (list->point4f (point4f->list v)))))))
(test "point4f ref" '(1.0 2.0 3.0)
      (lambda ()
        (let1 v (point4f 1 2 3)
          (map (pa$ point4f-ref v) '(0 1 2)))))
(test "point4f set" #,(point4f 3.0 2.0 1.0)
      (lambda ()
        (let1 v (point4f 0 0 0)
          (set! (point4f-ref v 0) 3.0)
          (set! (point4f-ref v 1) 2.0)
          (set! (point4f-ref v 2) 1.0)
          v)))
(test "point4f +" #,(point4f 3.0 5.0 7.0)
      (lambda ()
        (+ #,(point4f 1.0 2.0 3.0)
           #,(vector4f 2.0 3.0 4.0))))
(test "point4f +" #,(point4f 1.0 1.0 1.0)
      (lambda ()
        (+ #,(point4f 1.0 2.0 3.0)
           #,(vector4f 2.0 3.0 4.0)
           #,(vector4f -2.0 -4.0 -6.0))))
(test "point4f -" #,(point4f -1.0 -2.0 -3.0)
      (lambda ()
        (- #,(point4f 1.0 2.0 3.0)
           #,(vector4f 2.0 4.0 6.0))))
(test "point4f -" #,(vector4f -1.0 -2.0 -3.0)
      (lambda ()
        (- #,(point4f 1.0 2.0 3.0)
           #,(point4f 2.0 4.0 6.0))))

;;------------------------------------------------------------------
(test-section "matrix4f")

(test "matrix4f" #,(matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
      (lambda ()
        (matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)))
(test "matrix4f row" 
      '(#,(vector4f 0 4 8 2)
        #,(vector4f 1 5 9 3)
        #,(vector4f 2 6 0 4)
        #,(vector4f 3 7 1 5))
      (lambda ()
        (let1 m (matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
          (map (pa$ matrix4f-row m) '(0 1 2 3)))))
(test "matrix4f row set!"
      #,(matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
      (lambda ()
        (let1 m (make-matrix4f)
          (set! (matrix4f-row m 3) (vector4f 3 7 1 5))
          (set! (matrix4f-row m 2) (vector4f 2 6 0 4))
          (set! (matrix4f-row m 1) (vector4f 1 5 9 3))
          (set! (matrix4f-row m 0) (vector4f 0 4 8 2))
          m)))
(test "matrix4f column" 
      '(#,(vector4f 0 1 2 3)
        #,(vector4f 4 5 6 7)
        #,(vector4f 8 9 0 1)
        #,(vector4f 2 3 4 5))
      (lambda ()
        (let1 m (matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
          (map (pa$ matrix4f-column m) '(0 1 2 3)))))
(test "matrix4f column set!"
      #,(matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
      (lambda ()
        (let1 m (make-matrix4f)
          (set! (matrix4f-column m 3) (vector4f 2 3 4 5))
          (set! (matrix4f-column m 2) (vector4f 8 9 0 1))
          (set! (matrix4f-column m 1) (vector4f 4 5 6 7))
          (set! (matrix4f-column m 0) (vector4f 0 1 2 3))
          m)))

(test "matrix4f * matrix4f"
      (let ((A0 (vector4f 0 4 8 12))
            (A1 (vector4f 1 5 9 13))
            (A2 (vector4f 2 6 10 14))
            (A3 (vector4f 3 7 11 15))
            (B0 (vector4f 16 17 18 19))
            (B1 (vector4f 20 21 22 23))
            (B2 (vector4f 24 25 26 27))
            (B3 (vector4f 28 29 30 31))
            (dot vector4f-dot))
        (matrix4f (dot A0 B0) (dot A1 B0) (dot A2 B0) (dot A3 B0)
                  (dot A0 B1) (dot A1 B1) (dot A2 B1) (dot A3 B1)
                  (dot A0 B2) (dot A1 B2) (dot A2 B2) (dot A3 B2)
                  (dot A0 B3) (dot A1 B3) (dot A2 B3) (dot A3 B3)))
      (lambda ()
        (* (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
           (matrix4f 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)))
      )
               
(test "matrix4f * vector4f"
      (let ((V (vector4f 1 -2 3 -4)))
        (vector4f (vector4f-dot (vector4f 0 4 8 12) V)
                  (vector4f-dot (vector4f 1 5 9 13) V)
                  (vector4f-dot (vector4f 2 6 10 14) V)
                  (vector4f-dot (vector4f 3 7 11 15) V)))
      (lambda ()
        (* (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
           (vector4f 1 -2 3 -4))))

(test "matrix4f * scalar"
      (matrix4f 0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15)
      (lambda ()
        (* (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) -1)))

(test "matrix4f transpose"
      (matrix4f 0 4 8 12 1 5 9 13 2 6 10 14 3 7 11 15)
      (lambda ()
        (matrix4f-transpose
         (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))

(test "matrix4f transpose!"
      (matrix4f 0 4 8 12 1 5 9 13 2 6 10 14 3 7 11 15)
      (lambda ()
        (let1 m (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          (matrix4f-transpose! m)
          m)))

(test "matrix4f determinant" 1.0
      (lambda ()
        (matrix4f-determinant (matrix4f 1 0 0 0 0 2 4 9 0 3 2 6 0 2 3 7))))

(test "matrix4f determinant" -1.0
      (lambda ()
        (matrix4f-determinant (matrix4f 0 1 0 0 2 0 3 9 3 0 2 5 5 0 3 7))))

(test "matrix4f determinant" 1.0
      (lambda ()
        (matrix4f-determinant (matrix4f 0 0 1 0 2 3 0 17 3 2 0 11 6 3 0 16))))

(test "matrix4f determinant" -1.0
      (lambda ()
        (matrix4f-determinant (matrix4f 0 0 0 1 2 3 17 0 3 2 11 0 6 3 16 0))))

(test "matrix4f determinant" -1.0
      (lambda ()
        (matrix4f-determinant (matrix4f 0 2 4 9 1 0 0 0 0 3 2 6 0 2 3 7))))

(test "matrix4f determinant" 1.0
      (lambda ()
        (matrix4f-determinant (matrix4f 0 2 4 9 0 3 2 6 1 0 0 0 0 2 3 7))))

(test "matrix4f inverse"
      (matrix4f 1 0 0 0 0 -4 -1 6 0 -9 -4 15 0 5 2 -8)
      (lambda ()
        (matrix4f-inverse (matrix4f 1 0 0 0 0 2 4 9 0 3 2 6 0 2 3 7)))
      nearly=?)

(test "matrix4f inverse, mul"
      (matrix4f 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)
      (lambda ()
        (let ((m (matrix4f 0 2 4 9 0 3 2 6 1 0 0 0 0 2 3 7)))
          (matrix4f-mul m (matrix4f-inverse m))))
      nearly=?)
      
(test "matrix4f inverse, mul 2"
      (matrix4f 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)
      (lambda ()
        (let* ((m (matrix4f 0 2 4 9 0 3 2 6 1 0 0 0 0 2 3 7))
               (n (matrix4f 0 0 0 1 2 3 17 0 3 2 11 0 6 3 16 0))
               (1/m (matrix4f-inverse m))
               (1/n (matrix4f-inverse n)))
          (matrix4f-mul (matrix4f-mul m n)
                        (matrix4f-mul 1/n 1/m))))
      nearly=?)

(test "matrix4f inverse!"
      (matrix4f 0 -1 6 -3 1 0 0 0 0 4 -31 17 0 -1 9 -5)
      (lambda ()
        (let ((m (matrix4f 0 1 0 0 2 0 3 9 3 0 2 5 5 0 3 7)))
          (matrix4f-inverse! m)
          m))
      nearly=?)

(test "matrix4f inverse (singular)"
      #f
      (lambda ()
        (matrix4f-inverse (matrix4f 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) #f)))

(test-error "matrix4f inverse (singular)"
            (lambda ()
              (matrix4f-inverse (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))))

(test "matrix4 and transform"
      (let* ((phi (* 60 pi/180))
             (cosphi   (cos phi))
             (1-cosphi (- 1 cosphi))
             (sinphi   (sin phi))
             (sqrt15   (sqrt 15))
             (vx       (/ 1 sqrt15))
             (vy       (/ 2 sqrt15))
             (vz       (/ 3 sqrt15)))
        (matrix4f (* 1.1 (+ cosphi (* 1-cosphi vx vx)))
                  (+ (* 1-cosphi vx vy) (* sinphi vz))
                  (- (* 1-cosphi vx vz) (* sinphi vy))
                  0.0
                  (- (* 1-cosphi vx vy) (* sinphi vz))
                  (* 1.2 (+ cosphi (* 1-cosphi vy vy)))
                  (+ (* 1-cosphi vy vz) (* sinphi vx))
                  0.0
                  (+ (* 1-cosphi vx vz) (* sinphi vy))
                  (- (* 1-cosphi vy vz) (* sinphi vx))
                  (* 1.3 (+ cosphi (* 1-cosphi vz vz)))
                  0.0
                  -2.0
                  -3.0
                  -4.0
                  1.0))
      (lambda ()
        (trs->matrix4f #,(vector4f -2.0 -3.0 -4.0)
                       (vector4f (/ 1 (sqrt 15))
                                 (/ 2 (sqrt 15))
                                 (/ 3 (sqrt 15)))
                       (* 60 pi/180)
                       #,(vector4f 1.1 1.2 1.3)))
      nearly=?)

;;------------------------------------------------------------------
(test-section "quatf")

(test "conjugate"
      (make-quatf (vector4f 0 (/ 1 (sqrt 5)) (/ 2 (sqrt 5))) -0.5)
      (lambda ()
        (quatf-conjugate
         (make-quatf (vector4f 0 (/ 1 (sqrt 5)) (/ 2 (sqrt 5))) 0.5))))

(let ((p (make-quatf (vector4f-normalize (vector4f 3 2 1)) -1.6))
      (q (make-quatf (vector4f-normalize (vector4f 0 2 5)) 0.7))
      (r (make-quatf (vector4f-normalize (vector4f 2 1 3)) 0.1)))
  (test "add, sub, and mul"
        (list (+ p q) (- p q) (* p q) p)
        (lambda ()
          (let ((p+ (quatf-copy p))
                (p- (quatf-copy p))
                (p* (quatf-copy p)))
            (quatf-add! p+ q)
            (quatf-sub! p- q)
            (quatf-mul! p* q)
            (list p+ p- p* p)))))

(test "rotation -> quaterion"
      (let* ((phi (* 75 pi/180))
             (q   (quatf (* (sin phi) (/ 1 (sqrt 15)))
                         (* (sin phi) (/ 2 (sqrt 15)))
                         (* (sin phi) (/ 3 (sqrt 15)))
                         (cos phi))))
        (list q q))
      (lambda ()
        (let1 rotv (vector4f (/ 1 (sqrt 15))
                             (/ 2 (sqrt 15))
                             (/ 3 (sqrt 15)))
          (list (make-quatf rotv (* 150 pi/180))
                (let1 q (make-quatf)
                  (rotation->quatf! q rotv (* 150 pi/180))))))
      )


(test "transform by quaternion"
      (* (rotation->matrix4f (vector4f 0 (/ 1 (sqrt 5)) (/ 2 (sqrt 5)))
                             (* 15 pi/180))
         (point4f 3.1 2.1 1.1))
      (lambda ()
        (quatf-transform (make-quatf (vector4f 0 (/ 1 (sqrt 5)) (/ 2 (sqrt 5)))
                                     (* 15 pi/180))
                         (point4f 3.1 2.1 1.1)))
      nearly=?)

(test "tqs->matrix"
      (trs->matrix4f #,(vector4f 0 0 0)
                     (vector4f (/ 2 (sqrt 29))
                               (/ 3 (sqrt 29))
                               (/ 4 (sqrt 29)))
                     (* -41 pi/180)
                     #,(vector4f 1 2 3))
      (lambda ()
        (tqs->matrix4f #,(vector4f 0 0 0)
                       (make-quatf (vector4f (/ 2 (sqrt 29))
                                             (/ 3 (sqrt 29))
                                             (/ 4 (sqrt 29)))
                                   (* -41 pi/180))
                       #,(vector4f 1 2 3)))
      nearly=?)

(test-end)
