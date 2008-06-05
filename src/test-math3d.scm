;;
;; test for gl.math3d module
;;

(use gauche.test)
(add-load-path "../lib")

(test-start "gl.math3d")
(use gl.math3d)
(use gauche.sequence)
(use math.const)
(use srfi-1)

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

(test* "vector4f" #t
       (vector4f? (vector4f 0 1 2 3)))
(test* "vector4f reader" #t
       (equal? (vector4f 0 1 2 3) #,(vector4f 0 1 2 3)))
(test* "vector4f converters" #t
       (let1 v (vector4f 0 1 2 3)
         (and (equal? v (list->vector4f '(0 1 2 3)))
              (equal? v (f32vector->vector4f '#f32(0.0 1.0 2.0 3.0)))
              (equal? v (list->vector4f (vector4f->list v)))
              (equal? v (f32vector->vector4f (vector4f->f32vector v)))
              (equal? v (coerce-to <vector4f> '(0 1 2 3)))
              (equal? v (coerce-to <vector4f> '#(0 1 2 3)))
              (equal? v (coerce-to <vector4f> '#f32(0 1 2 3)))
              )))
(test* "vector4f ref" '(0.0 1.0 2.0 3.0)
       (let1 v (vector4f 0 1 2 3)
         (map (pa$ vector4f-ref v) '(0 1 2 3))))
(test* "vector4f set" #,(vector4f 3.0 2.0 1.0 0.0)
       (let1 v (vector4f 0 0 0 0)
         (set! (vector4f-ref v 0) 3.0)
         (set! (vector4f-ref v 1) 2.0)
         (set! (vector4f-ref v 2) 1.0)
         (set! (vector4f-ref v 3) 0.0)
         v))
(test* "vector4f +" #,(vector4f 3.0 5.0 7.0 9.0)
       (+ #,(vector4f 1.0 2.0 3.0 4.0)
          #,(vector4f 2.0 3.0 4.0 5.0)))
(test* "vector4f +" #,(vector4f 1.0 1.0 1.0 1.0)
       (+ #,(vector4f 1.0 2.0 3.0 4.0)
          #,(vector4f 2.0 3.0 4.0 5.0)
          #,(vector4f -2.0 -4.0 -6.0 -8.0)))
(test* "vector4f -" #,(vector4f -1.0 -2.0 -3.0 -4.0)
       (- #,(vector4f 1.0 2.0 3.0 4.0)
          #,(vector4f 2.0 4.0 6.0 8.0)))
(test* "vector4f *" #,(vector4f 2 4 6 8)
       (* #,(vector4f 1 2 3 4) 2.0))
(test* "vector4f *" #,(vector4f 2 4 6 8)
       (* 2.0 #,(vector4f 1 2 3 4)))
(test* "vector4f /" #,(vector4f 0.5 1.0 1.5 2.0)
       (/ #,(vector4f 1 2 3 4) 2.0))
(test* "vector4f dot" 40.0
       (vector4f-dot #,(vector4f 1.0 2.0 3.0 4.0)
                     #,(vector4f 2.0 3.0 4.0 5.0)))
(test* "vector4f cross" #,(vector4f -4.0 8.0 -4.0 0.0)
       (vector4f-cross #,(vector4f 1.0 2.0 3.0 0.0)
                       #,(vector4f 5.0 6.0 7.0 0.0)))
(test* "vector4f normalize" #,(vector4f 0.5 0.5 0.5 0.5)
       (vector4f-normalize (vector4f 1 1 1 1)))
(test* "vector4f normalize!" #,(vector4f 0.5 0.5 0.5 0.5)
       (let1 v (vector4f 4 4 4 4)
         (vector4f-normalize! v)
         v))


;; sequence access
(test* "sequence"
       '(1.0 2.0 3.0 4.0)
       (coerce-to <list> #,(vector4f 1.0 2.0 3.0 4.0)))

(test* "sequence"
       #,(vector4f 1.0 2.0 3.0 4.0)
       (coerce-to <vector4f> '(1.0 2.0 3.0 4.0)))

;;------------------------------------------------------------------
(test-section "point4f")

(test* "point4f" #t
       (point4f? (point4f 1 2 3)))
(test* "point4f reader" #t
       (equal? (point4f 1 2 3) #,(point4f 1 2 3)))
(test* "point4f converters" #t
       (let1 v (point4f 1 2 3)
         (and (equal? v (list->point4f '(1 2 3)))
              (equal? v (list->point4f (point4f->list v))))))
(test* "point4f ref" '(1.0 2.0 3.0)
       (let1 v (point4f 1 2 3)
         (map (pa$ point4f-ref v) '(0 1 2))))
(test* "point4f set" #,(point4f 3.0 2.0 1.0)
       (let1 v (point4f 0 0 0)
         (set! (point4f-ref v 0) 3.0)
         (set! (point4f-ref v 1) 2.0)
         (set! (point4f-ref v 2) 1.0)
         v))
(test* "point4f +" #,(point4f 3.0 5.0 7.0)
       (+ #,(point4f 1.0 2.0 3.0)
          #,(vector4f 2.0 3.0 4.0)))
(test* "point4f +" #,(point4f 1.0 1.0 1.0)
       (+ #,(point4f 1.0 2.0 3.0)
          #,(vector4f 2.0 3.0 4.0)
          #,(vector4f -2.0 -4.0 -6.0)))
(test* "point4f -" #,(point4f -1.0 -2.0 -3.0)
       (- #,(point4f 1.0 2.0 3.0)
          #,(vector4f 2.0 4.0 6.0)))
(test* "point4f -" #,(vector4f -1.0 -2.0 -3.0)
       (- #,(point4f 1.0 2.0 3.0)
          #,(point4f 2.0 4.0 6.0)))

;; sequence access
(test* "sequence"
       '(1.0 2.0 3.0 4.0)
       (coerce-to <list> #,(point4f 1.0 2.0 3.0 4.0)))

(test* "sequence"
       #,(point4f 1.0 2.0 3.0 4.0)
       (coerce-to <point4f> '(1.0 2.0 3.0 4.0)))

;;------------------------------------------------------------------
(test-section "matrix4f")

(test* "matrix4f" #,(matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
       (matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5))
(test* "matrix4f row" 
       '(#,(vector4f 0 4 8 2)
         #,(vector4f 1 5 9 3)
         #,(vector4f 2 6 0 4)
         #,(vector4f 3 7 1 5))
       (let1 m (matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
         (map (pa$ matrix4f-row m) '(0 1 2 3))))
(test* "matrix4f row set!"
       #,(matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
       (let1 m (make-matrix4f)
         (set! (matrix4f-row m 3) (vector4f 3 7 1 5))
         (set! (matrix4f-row m 2) (vector4f 2 6 0 4))
         (set! (matrix4f-row m 1) (vector4f 1 5 9 3))
         (set! (matrix4f-row m 0) (vector4f 0 4 8 2))
         m))
(test* "matrix4f column" 
       '(#,(vector4f 0 1 2 3)
         #,(vector4f 4 5 6 7)
         #,(vector4f 8 9 0 1)
         #,(vector4f 2 3 4 5))
       (let1 m (matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
         (map (pa$ matrix4f-column m) '(0 1 2 3))))
(test* "matrix4f column set!"
       #,(matrix4f 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5)
       (let1 m (make-matrix4f)
         (set! (matrix4f-column m 3) (vector4f 2 3 4 5))
         (set! (matrix4f-column m 2) (vector4f 8 9 0 1))
         (set! (matrix4f-column m 1) (vector4f 4 5 6 7))
         (set! (matrix4f-column m 0) (vector4f 0 1 2 3))
         m))

(test* "matrix4f * matrix4f"
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
       (* (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          (matrix4f 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
       )

(test* "matrix4f * vector4f"
       (let ((V (vector4f 1 -2 3 -4)))
         (vector4f (vector4f-dot (vector4f 0 4 8 12) V)
                   (vector4f-dot (vector4f 1 5 9 13) V)
                   (vector4f-dot (vector4f 2 6 10 14) V)
                   (vector4f-dot (vector4f 3 7 11 15) V)))
       (* (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
          (vector4f 1 -2 3 -4)))

(test* "matrix4f * scalar"
       (matrix4f 0 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15)
       (* (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) -1))

(test* "matrix4f transpose"
       (matrix4f 0 4 8 12 1 5 9 13 2 6 10 14 3 7 11 15)
       (matrix4f-transpose
        (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

(test* "matrix4f transpose!"
       (matrix4f 0 4 8 12 1 5 9 13 2 6 10 14 3 7 11 15)
       (let1 m (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
         (matrix4f-transpose! m)
         m))

(test* "matrix4f determinant" 1.0
       (matrix4f-determinant (matrix4f 1 0 0 0 0 2 4 9 0 3 2 6 0 2 3 7)))

(test* "matrix4f determinant" -1.0
       (matrix4f-determinant (matrix4f 0 1 0 0 2 0 3 9 3 0 2 5 5 0 3 7)))

(test* "matrix4f determinant" 1.0
       (matrix4f-determinant (matrix4f 0 0 1 0 2 3 0 17 3 2 0 11 6 3 0 16)))

(test* "matrix4f determinant" -1.0
       (matrix4f-determinant (matrix4f 0 0 0 1 2 3 17 0 3 2 11 0 6 3 16 0)))

(test* "matrix4f determinant" -1.0
       (matrix4f-determinant (matrix4f 0 2 4 9 1 0 0 0 0 3 2 6 0 2 3 7)))

(test* "matrix4f determinant" 1.0
       (matrix4f-determinant (matrix4f 0 2 4 9 0 3 2 6 1 0 0 0 0 2 3 7)))

(test* "matrix4f inverse"
       (matrix4f 1 0 0 0 0 -4 -1 6 0 -9 -4 15 0 5 2 -8)
       (matrix4f-inverse (matrix4f 1 0 0 0 0 2 4 9 0 3 2 6 0 2 3 7))
       nearly=?)

(test* "matrix4f inverse, mul"
       (matrix4f 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)
       (let ((m (matrix4f 0 2 4 9 0 3 2 6 1 0 0 0 0 2 3 7)))
         (matrix4f-mul m (matrix4f-inverse m)))
       nearly=?)

(test* "matrix4f inverse, mul 2"
       (matrix4f 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)
       (let* ((m (matrix4f 0 2 4 9 0 3 2 6 1 0 0 0 0 2 3 7))
              (n (matrix4f 0 0 0 1 2 3 17 0 3 2 11 0 6 3 16 0))
              (1/m (matrix4f-inverse m))
              (1/n (matrix4f-inverse n)))
         (matrix4f-mul (matrix4f-mul m n)
                       (matrix4f-mul 1/n 1/m)))
       nearly=?)

(test* "matrix4f inverse!"
       (matrix4f 0 -1 6 -3 1 0 0 0 0 4 -31 17 0 -1 9 -5)
       (let ((m (matrix4f 0 1 0 0 2 0 3 9 3 0 2 5 5 0 3 7)))
         (matrix4f-inverse! m)
         m)
       nearly=?)

(test* "matrix4f inverse (singular)"
       #f
       (matrix4f-inverse (matrix4f 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) #f))

(test* "matrix4f inverse (singular)" *test-error*
       (matrix4f-inverse (matrix4f 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))

(let ((t0 -2.0) (t1 -3.0) (t2 -4.0)
      (phi (* 60 pi/180))
      (s0  1.1) (s1  1.2) (s2  1.3))
  (test* "matrix4 and transform"
         (let* ((cosphi   (cos phi))
                (1-cosphi (- 1 cosphi))
                (sinphi   (sin phi))
                (sqrt14   (sqrt 14))
                (vx       (/ 1 sqrt14))
                (vy       (/ 2 sqrt14))
                (vz       (/ 3 sqrt14))
                (T (matrix4f 1 0 0 0 0 1 0 0 0 0 1 0 t0 t1 t2 1))
                (R (matrix4f (+ cosphi (* 1-cosphi vx vx))
                             (+ (* 1-cosphi vx vy) (* sinphi vz))
                             (- (* 1-cosphi vx vz) (* sinphi vy))
                             0.0
                             (- (* 1-cosphi vx vy) (* sinphi vz))
                             (+ cosphi (* 1-cosphi vy vy))
                             (+ (* 1-cosphi vy vz) (* sinphi vx))
                             0.0
                             (+ (* 1-cosphi vx vz) (* sinphi vy))
                             (- (* 1-cosphi vy vz) (* sinphi vx))
                             (+ cosphi (* 1-cosphi vz vz))
                             0.0
                             0.0 0.0 0.0 1.0))
                (S (matrix4f s0 0 0 0 0 s1 0 0 0 0 s2 0 0 0 0 1))
                )
           (matrix4f-mul T (matrix4f-mul R S)))
         (trs->matrix4f (vector4f t0 t1 t2)
                        (vector4f (/ 1 (sqrt 14))
                                  (/ 2 (sqrt 14))
                                  (/ 3 (sqrt 14)))
                        phi
                        (vector4f s0 s1 s2))
         nearly=?))

;; tests euler angles
(let* ((rotx (* 23 pi/180))
       (roty (* -66 pi/180))
       (rotz (* 171 pi/180))
       (Rx (rotation->matrix4f (vector4f 1 0 0) rotx))
       (Ry (rotation->matrix4f (vector4f 0 1 0) roty))
       (Rz (rotation->matrix4f (vector4f 0 0 1) rotz)))
  (define (euler-tester sig a b c)
    (test #`"euler->mat (,sig)" (matrix4f-mul c (matrix4f-mul b a))
          (lambda () (euler-angle->matrix4f rotx roty rotz sig))
          nearly=?))
  (euler-tester 'xyz Rx Ry Rz)
  (euler-tester 'xzy Rx Rz Ry)
  (euler-tester 'yzx Ry Rz Rx)
  (euler-tester 'yxz Ry Rx Rz)
  (euler-tester 'zxy Rz Rx Ry)
  (euler-tester 'zyx Rz Ry Rx)
  )

;; matrix decompose
(define (matrix-decompose-tester name T R H S)
  (test name '(#t #t #t #t #t)
        (lambda ()
          (let* ((tmat (translation->matrix4f T))
                 (smat (scale->matrix4f S))
                 (mat  (matrix4f-mul tmat (matrix4f-mul R smat))))
            (receive (f t r h s)
                (matrix4f-decompose mat)
              ;(print r)
              ;(print R)
              ;(print h)
              ;(print s)
              (list f
                    (nearly=? t T)
                    (nearly=? r R)
                    (nearly=? h H)
                    (nearly=? s S)))))))

(matrix-decompose-tester
 "matrix-decompose"
 (vector4f 1 3 2)
 (rotation->matrix4f (vector4f-normalize (vector4f 3 8 -1)) (* 264 pi/180))
 (vector4f 0 0 0 0)
 (vector4f 1.1 0.5 0.2))

(matrix-decompose-tester
 "matrix-decompose (flip)"
 (vector4f 9 2 4)
 (rotation->matrix4f (vector4f-normalize (vector4f 6 0 2)) (* 82 pi/180))
 (vector4f 0 0 0 0)
 (vector4f -1 -2 -3))

(test* "matrix4f->rotation" #t
       (let* ((axis  (vector4f-normalize (vector4f 1 2 3)))
              (angle (* 45 pi/180))
              (m     (rotation->matrix4f axis angle)))
         (receive (raxis rangle)
             (matrix4f->rotation m)
           (and (nearly=? axis raxis)
                (< (abs (- rangle angle)) 1.0e-4)))))

;; sequence access
(test* "sequence"
       '(1.0 2.0 3.0 4.0 -1.0 -2.0 -3.0 -4.0 4.0 3.0 2.0 1.0 -4.0 -3.0 -2.0 -1.0)
       (coerce-to <list>
                  #,(matrix4f 1 2 3 4 -1 -2 -3 -4 4 3 2 1 -4 -3 -2 -1)))

(test* "sequence"
       #,(matrix4f 1.0 2.0 3.0 4.0 -1.0 -2.0 -3.0 -4.0 4.0 3.0 2.0 1.0 -4.0 -3.0 -2.0 -1.0)
       (coerce-to <matrix4f>
                  '(1.0 2.0 3.0 4.0 -1.0 -2.0 -3.0 -4.0 4.0 3.0 2.0 1.0 -4.0 -3.0 -2.0 -1.0)))

;;------------------------------------------------------------------
(test-section "quatf")

(test* "conjugate"
       (make-quatf (vector4f 0 (/ 1 (sqrt 5)) (/ 2 (sqrt 5))) -0.5)
       (quatf-conjugate
        (make-quatf (vector4f 0 (/ 1 (sqrt 5)) (/ 2 (sqrt 5))) 0.5)))

(let ((p (make-quatf (vector4f-normalize (vector4f 3 2 1)) -1.6))
      (q (make-quatf (vector4f-normalize (vector4f 0 2 5)) 0.7))
      (r (make-quatf (vector4f-normalize (vector4f 2 1 3)) 0.1)))
  (test* "add, sub, and mul"
         (list (+ p q) (- p q) (* p q) p)
         (let ((p+ (quatf-copy p))
               (p- (quatf-copy p))
               (p* (quatf-copy p)))
           (quatf-add! p+ q)
           (quatf-sub! p- q)
           (quatf-mul! p* q)
           (list p+ p- p* p))))

(test* "rotation -> quaterion"
       (let* ((phi (* 75 pi/180))
              (q   (quatf (* (sin phi) (/ 1 (sqrt 14)))
                          (* (sin phi) (/ 2 (sqrt 14)))
                          (* (sin phi) (/ 3 (sqrt 14)))
                          (cos phi))))
         (list q q))
       (let1 rotv (vector4f (/ 1 (sqrt 14))
                            (/ 2 (sqrt 14))
                            (/ 3 (sqrt 14)))
         (list (make-quatf rotv (* 150 pi/180))
               (let1 q (make-quatf)
                 (rotation->quatf! q rotv (* 150 pi/180)))))
       )


(test* "transform by quaternion"
       (* (rotation->matrix4f (vector4f 0 (/ 1 (sqrt 5)) (/ 2 (sqrt 5)))
                              (* 15 pi/180))
          (point4f 3.1 2.1 1.1))
       (quatf-transform (make-quatf (vector4f 0 (/ 1 (sqrt 5)) (/ 2 (sqrt 5)))
                                    (* 15 pi/180))
                        (point4f 3.1 2.1 1.1))
       nearly=?)

(test* "tqs->matrix"
       (trs->matrix4f #,(vector4f 0 0 0)
                      (vector4f (/ 2 (sqrt 29))
                                (/ 3 (sqrt 29))
                                (/ 4 (sqrt 29)))
                      (* -41 pi/180)
                      #,(vector4f 1 2 3))
       (tqs->matrix4f #,(vector4f 0 0 0)
                      (make-quatf (vector4f (/ 2 (sqrt 29))
                                            (/ 3 (sqrt 29))
                                            (/ 4 (sqrt 29)))
                                  (* -41 pi/180))
                      #,(vector4f 1 2 3))
       nearly=?)

;; matrix <-> quaternion
(test* "quatf->matrix"
       (rotation->matrix4f (vector4f (/ 3 (sqrt 14))
                                     (/ 2 (sqrt 14))
                                     (/ -1 (sqrt 14)))
                           (* 75 pi/180))
       (quatf->matrix4f (make-quatf (vector4f (/ 3 (sqrt 14))
                                              (/ 2 (sqrt 14))
                                              (/ -1 (sqrt 14)))
                                    (* 75 pi/180)))
       nearly=?)

(test* "matrix->quatf"
       (make-quatf (vector4f (/ 7 (sqrt 66))
                             (/ -1 (sqrt 66))
                             (/ 4 (sqrt 66)))
                   (* -13 pi/180))
       (matrix4f->quatf (rotation->matrix4f (vector4f (/ 7 (sqrt 66))
                                                      (/ -1 (sqrt 66))
                                                      (/ 4 (sqrt 66)))
                                            (* -13 pi/180)))
       nearly=?)

;; rotation check
(let ()
  (define (rot-test q v)
    (let* ((nv (vector4f-normalize v)))
      (test* (format "rotation by quaternion ~s ~s" q v)
             (* (quatf->matrix4f q) nv)
             (quatf-transform q nv)
             nearly=?)))
  (define (rot-test* q)
    (for-each (cute rot-test (quatf-normalize q) <>)
              '(#,(vector4f 1 0 0 0)
                #,(vector4f 0 1 0 0)
                #,(vector4f 0 0 1 0)
                #,(vector4f 1 1 0 0)
                #,(vector4f 1 -1 0 0)
                #,(vector4f -1 0 1 0)
                #,(vector4f 1 0 -1 0)
                #,(vector4f 0 1 -1 0)
                #,(vector4f 0 -1 1 0)
                #,(vector4f 3 1 4 0))))
  (for-each rot-test*
            '(#,(quatf 1 0 0 0) #,(quatf 0 1 0 0) #,(quatf 0 0 1 0)
              #,(quatf 0 0 0 1) #,(quatf 1 1 1 1) #,(quatf 1 -1 1 -1)
              #,(quatf 3 1 -4 5))))

;; test case for small trace case
(test* "matrix->quatf (small trace)"
       (make-quatf (vector4f 1 0 0) (- pi 0.1))
       (matrix4f->quatf (rotation->matrix4f (vector4f 1 0 0) (- pi 0.1)))
       nearly=?)
(test* "matrix->quatf (small trace)"
       (make-quatf (vector4f 0 1 0) (- pi 0.1))
       (matrix4f->quatf (rotation->matrix4f (vector4f 0 1 0) (- pi 0.1)))
       nearly=?)
(test* "matrix->quatf (small trace)"
       (make-quatf (vector4f 0 0 1) (- pi 0.1))
       (matrix4f->quatf (rotation->matrix4f (vector4f 0 0 1) (- pi 0.1)))
       nearly=?)

;; two vectors -> quatf
(let ()
  (define (2vtest v w)
    (let ((nv (vector4f-normalize v))
          (nw (vector4f-normalize w)))
      (test* (format "2vtest ~s ~s" v w) nw
             (quatf-transform (2vectors->quatf nv nw) nv)
             nearly=?)
      (test* (format "2vtest ~s ~s" w v) nv
             (quatf-transform (2vectors->quatf nw nv) nw)
             nearly=?)))
  (2vtest #,(vector4f 1 0 0 0) #,(vector4f 0 1 0 0))
  (2vtest #,(vector4f 0 1 0 0) #,(vector4f 0 0 1 0))
  (2vtest #,(vector4f 0 0 1 0) #,(vector4f 1 0 0 0))
  (2vtest #,(vector4f 1 2 3 0) #,(vector4f 4 -5 6 0))
  (2vtest #,(vector4f 1 1 0 0) #,(vector4f 1 1 0 0))
  (2vtest #,(vector4f 1 1 0 0) #,(vector4f 1 1 0.001 0))
  )

;; four vectors -> quatf
(let ()
  (define (4vtest v1 v2 w1 w2)
    (let ((nv1 (vector4f-normalize v1))
          (nv2 (vector4f-normalize v2))
          (nw1 (vector4f-normalize w1))
          (nw2 (vector4f-normalize w2)))
      (test* (format "4vtest (~s ~s) (~s ~s)" v1 v2 w1 w2)
             (list nw1 nw2 (+ nw1 nw2))
             (let1 q (4vectors->quatf nv1 nv2 nw1 nw2)
               (list (quatf-transform q nv1)
                     (quatf-transform q nv2)
                     (quatf-transform q (+ nv1 nv2))))
             (cut every nearly=? <> <>))
      (test* (format "4vtest (~s ~s) (~s ~s)" w1 w2 v1 v2)
             (list nv1 nv2 (+ nv1 nv2))
             (let1 q (4vectors->quatf nw1 nw2 nv1 nv2)
               (list (quatf-transform q nw1)
                     (quatf-transform q nw2)
                     (quatf-transform q (+ nw1 nw2))))
             (cut every nearly=? <> <>))))

  (4vtest #,(vector4f 1 0 0 0) #,(vector4f 0 1 0 0)
          #,(vector4f 0 1 0 0) #,(vector4f 0 0 1 0))
  (4vtest #,(vector4f 1 0 0 0) #,(vector4f 0 1 0 0)
          #,(vector4f 0 0 -1 0) #,(vector4f 0 1 0 0))
  (4vtest #,(vector4f 1 1 0 0) #,(vector4f 1 -1 0)
          #,(vector4f 1 0 0) #,(vector4f 0 0 1 0))
  )

;; sequence access
(test* "sequence"
       '(1.0 2.0 3.0 4.0)
       (coerce-to <list> #,(quatf 1.0 2.0 3.0 4.0)))

(test* "sequence"
       #,(quatf 1.0 2.0 3.0 4.0)
       (coerce-to <quatf> '(1.0 2.0 3.0 4.0)))


(test-end)
