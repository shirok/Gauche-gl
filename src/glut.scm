;;;
;;; glut.scm - Gauche GLUT binding
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: glut.scm,v 1.2 2001-09-30 03:37:56 shirok Exp $
;;;

(define-module gl.glut
  (export-all)
  )

(select-module gl.glut)

(dynamic-load "gauche-glut")

(provide "gl/glut")
