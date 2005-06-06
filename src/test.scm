;; test for gl module

;; Note: this only tests if the gl and glut module is loadable or not,
;; and some basic tests.

(use gauche.test)
(use gauche.sequence)
(add-load-path "../lib")


(test-start "GL")

(test-section "loading gl")
(use gl)

(test-section "loading glut")
(use gl.glut)

(test-section "gl-boolean-vector")

(test* "constructor, predicate and length" (list #t 5)
       (let ((v (make-gl-boolean-vector 5)))
         (list (gl-boolean-vector? v)
               (gl-boolean-vector-length v))))

(test* "ref" '(#t #f #f #t)
       (let ((v (gl-boolean-vector #t #f #f #t)))
         (list (ref v 0)
               (ref v 1)
               (ref v 2)
               (ref v 3))))

(test* "set!" '(#t #f #f)
       (let ((v (make-gl-boolean-vector 3)))
         (set! (ref v 0) #t)
         (set! (ref v 1) #f)
         (set! (ref v 2) #f)
         (coerce-to <list> v)))

(test* "coercion & reader" #,(gl-boolean-vector #f #t #f #f #t)
       (coerce-to <gl-boolean-vector> '(#f #t #f #f #t)))

(test-end)

