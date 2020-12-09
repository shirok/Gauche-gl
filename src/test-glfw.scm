(use gauche.test)
(add-load-path "../lib" :relative)

(test-start "GLFW")

(use gl.glfw)
(test-module 'gl.glfw)


(test-end)

