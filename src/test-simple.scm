;;
;; test for gl.simple.* module
;;

(use gauche.test)
(add-load-path "../lib")

(test-start "gl.simple.*")

(test-section "gl.simple.viewer")

(use gl.simple.viewer)
(test-module 'gl.simple.viewer)

(test-section "gl.simple.image")

(use gl.simple.image)
(test-module 'gl.simple.image)

(test-section "gl.simple.scene-time")

(use gl.simple.scene-time)
(test-module 'gl.simple.scene-time)

(test-end)
