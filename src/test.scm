;; test for gl module

;; Note: this only tests if the gl and glut module is loadable or not.

(use gauche.test)
(add-load-path "../lib")

(test-start "GL")

(test-section "math3d")
(use gl.math3d)


(test-section "gl")
(use gl)

(test-section "glut")
(use gl.glut)

(test-end)

