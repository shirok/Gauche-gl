(use gauche.test)
(add-load-path "../lib" :relative)

(test-start "Vulkan")

(use gl.vulkan)
(test-module 'gl.vulkan)


(test-end)
