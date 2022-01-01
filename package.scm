;;
;; Gauche-gl package description
;;

(define-gauche-package "Gauche-gl"
  :version "0.7_pre1"
  :description "Graphics library binding for Gauche\n\
                This package provides binginds for OpenGL, GLUT, GLEW and Cg,\
                plus some additional libraries useful for graphics programming."
  :require (("Gauche" (>= "0.9.11")))
  :providing-modules (gl gl.glut gl.math3d
                         gl.simple.image gl.simple.viewer
                         gl.glfw gl.cg)
  :authors ("Shiro Kawai <shiro@acm.org>")
  :maintainers ()
  :licenses ("BSD")
  :homepage "https://practical-scheme.net/gauche"
  :repository "https://github.com/shirok/Gauche-gl.git"
  )
