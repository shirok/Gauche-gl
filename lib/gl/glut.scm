;;;
;;; glut.scm - Gauche GLUT binding
;;;
;;;  Copyright (c) 2001-2008  Shiro Kawai  <shiro@acm.org>
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: glut.scm,v 1.3 2008-06-04 11:46:21 shirok Exp $
;;;

(define-module gl.glut
  (export-all)
  )

(select-module gl.glut)

(dynamic-load "libgauche-glut")

(provide "gl/glut")
