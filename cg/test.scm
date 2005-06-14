;; test for gl.cg

(use gauche.test)
(add-load-path "../lib")
(add-load-path "../src")


(test-start "gl.cg")

(test-section "loading cg")
(use gl.cg)
(test-module 'gl.cg)

(test-end)
